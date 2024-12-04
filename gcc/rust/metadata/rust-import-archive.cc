// import-archive.cc -- Go frontend read import data from an archive file.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "rust-system.h"
#include "rust-diagnostics.h"
#include "rust-imports.h"

#ifndef O_BINARY
#define O_BINARY 0
#endif

// Archive magic numbers.

static const char armag[] = {'!', '<', 'a', 'r', 'c', 'h', '>', '\n'};
static const char armagt[] = {'!', '<', 't', 'h', 'i', 'n', '>', '\n'};
static const char armagb[] = {'<', 'b', 'i', 'g', 'a', 'f', '>', '\n'};
static const char arfmag[2] = {'`', '\n'};

namespace Rust {

// Archive fixed length header for AIX big format.

struct Archive_fl_header
{
  // Archive magic string.
  char fl_magic[8];
  // Offset to member table.
  char fl_memoff[20];
  // Offset to global symbol table.
  char fl_gstoff[20];
  // Offset to global symbol table for 64-bit objects.
  char fl_gst64off[20];
  // Offset to first archive member.
  char fl_fstmoff[20];
  // Offset to last archive member.
  char fl_lstmoff[20];
  // Offset to first member on free list.
  char fl_freeoff[20];
};

// The header of an entry in an archive.  This is all readable text,
// padded with spaces where necesary.

struct Archive_header
{
  // The entry name.
  char ar_name[16];
  // The file modification time.
  char ar_date[12];
  // The user's UID in decimal.
  char ar_uid[6];
  // The user's GID in decimal.
  char ar_gid[6];
  // The file mode in octal.
  char ar_mode[8];
  // The file size in decimal.
  char ar_size[10];
  // The final magic code.
  char ar_fmag[2];
};

// The header of an entry in an AIX big archive.
// This is followed by ar_namlen bytes + 2 bytes for arfmag.

struct Archive_big_header
{
  // The file size in decimal.
  char ar_size[20];
  // The next member offset in decimal.
  char ar_nxtmem[20];
  // The previous member offset in decimal.
  char ar_prvmem[20];
  // The file modification time in decimal.
  char ar_date[12];
  // The user's UID in decimal.
  char ar_uid[12];
  // The user's GID in decimal.
  char ar_gid[12];
  // The file mode in octal.
  char ar_mode[12];
  // The file name length in decimal.
  char ar_namlen[4];
};

// Return true if BYTES, which are from the start of the file, are an
// archive magic number.

bool
Import::is_archive_magic (const char *bytes)
{
  const int archive_magic_len = 8;
  return (memcmp (bytes, armag, archive_magic_len) == 0
	  || memcmp (bytes, armagt, archive_magic_len) == 0
	  || memcmp (bytes, armagb, archive_magic_len) == 0);
}

// An object used to read an archive file.

class Archive_file
{
public:
  Archive_file (const std::string &filename, int fd, location_t location)
    : filename_ (filename), fd_ (fd), filesize_ (-1), first_member_offset_ (0),
      extended_names_ (), is_thin_archive_ (false), is_big_archive_ (false),
      location_ (location), nested_archives_ ()
  {}

  // Initialize.
  bool initialize ();

  // Return the file name.
  const std::string &filename () const { return this->filename_; }

  // Get the file size.
  off_t filesize () const { return this->filesize_; }

  // Return the offset of the first member.
  off_t first_member_offset () const { return this->first_member_offset_; }

  // Return whether this is a thin archive.
  bool is_thin_archive () const { return this->is_thin_archive_; }

  // Return whether this is a big archive.
  bool is_big_archive () const { return this->is_big_archive_; }

  // Return the location of the import statement.
  location_t location () const { return this->location_; }

  // Read bytes.
  bool read (off_t offset, off_t size, char *);

  // Parse a decimal in readable text.
  bool parse_decimal (const char *str, off_t size, long *res) const;

  // Read the archive header at OFF, setting *PNAME, *SIZE,
  // *NESTED_OFF and *NEXT_OFF.
  bool read_header (off_t off, std::string *pname, off_t *size,
		    off_t *nested_off, off_t *next_off);

  // Interpret the header of HDR, the header of the archive member at
  // file offset OFF.  Return whether it succeeded.  Set *SIZE to the
  // size of the member.  Set *PNAME to the name of the member.  Set
  // *NESTED_OFF to the offset in a nested archive.
  bool interpret_header (const Archive_header *hdr, off_t off,
			 std::string *pname, off_t *size,
			 off_t *nested_off) const;

  // Get the file and offset for an archive member.
  bool get_file_and_offset (off_t off, const std::string &hdrname,
			    off_t nested_off, int *memfd, off_t *memoff,
			    std::string *memname);

private:
  // Initialize a big archive (AIX)
  bool initialize_big_archive ();

  // Initialize a normal archive
  bool initialize_archive ();

  // Read the big archive header at OFF, setting *PNAME, *SIZE and *NEXT_OFF.
  bool read_big_archive_header (off_t off, std::string *pname, off_t *size,
				off_t *next_off);

  // Read the normal archive header at OFF, setting *PNAME, *SIZE,
  // *NESTED_OFF and *NEXT_OFF.
  bool read_archive_header (off_t off, std::string *pname, off_t *size,
			    off_t *nested_off, off_t *next_off);

  // For keeping track of open nested archives in a thin archive file.
  typedef std::map<std::string, Archive_file *> Nested_archive_table;

  // The name of the file.
  std::string filename_;
  // The file descriptor.
  int fd_;
  // The file size;
  off_t filesize_;
  // The first member offset;
  off_t first_member_offset_;
  // The extended name table.
  std::string extended_names_;
  // Whether this is a thin archive.
  bool is_thin_archive_;
  // Whether this is a big archive.
  bool is_big_archive_;
  // The location of the import statements.
  location_t location_;
  // Table of nested archives.
  Nested_archive_table nested_archives_;
};

bool
Archive_file::initialize ()
{
  struct stat st;
  if (fstat (this->fd_, &st) < 0)
    {
      rust_error_at (this->location_, "%s: %m", this->filename_.c_str ());
      return false;
    }
  this->filesize_ = st.st_size;

  char buf[sizeof (armagt)];
  if (::lseek (this->fd_, 0, SEEK_SET) < 0
      || ::read (this->fd_, buf, sizeof (armagt)) != sizeof (armagt))
    {
      rust_error_at (this->location_, "%s: %m", this->filename_.c_str ());
      return false;
    }
  if (memcmp (buf, armagt, sizeof (armagt)) == 0)
    this->is_thin_archive_ = true;
  else if (memcmp (buf, armagb, sizeof (armagb)) == 0)
    this->is_big_archive_ = true;

  if (this->is_big_archive_)
    return this->initialize_big_archive ();
  else
    return this->initialize_archive ();
}

// Initialize a big archive (AIX).

bool
Archive_file::initialize_big_archive ()
{
  Archive_fl_header flhdr;

  // Read the fixed length header.
  if (::lseek (this->fd_, 0, SEEK_SET) < 0
      || ::read (this->fd_, &flhdr, sizeof (flhdr)) != sizeof (flhdr))
    {
      rust_error_at (this->location_, "%s: could not read archive header",
		     this->filename_.c_str ());
      return false;
    }

  // Parse offset of the first member.
  long off;
  if (!this->parse_decimal (flhdr.fl_fstmoff, sizeof (flhdr.fl_fstmoff), &off))
    {
      char *buf = new char[sizeof (flhdr.fl_fstmoff) + 1];
      memcpy (buf, flhdr.fl_fstmoff, sizeof (flhdr.fl_fstmoff));
      rust_error_at (this->location_,
		     ("%s: malformed first member offset in archive header"
		      " (expected decimal, got %s)"),
		     this->filename_.c_str (), buf);
      delete[] buf;
      return false;
    }
  if (off == 0) // Empty archive.
    this->first_member_offset_ = this->filesize_;
  else
    this->first_member_offset_ = off;
  return true;
}

// Initialize a normal archive.

bool
Archive_file::initialize_archive ()
{
  this->first_member_offset_ = sizeof (armag);
  if (this->first_member_offset_ == this->filesize_)
    {
      // Empty archive.
      return true;
    }

  // Look for the extended name table.
  std::string filename;
  off_t size;
  off_t next_off;
  if (!this->read_header (this->first_member_offset_, &filename, &size, NULL,
			  &next_off))
    return false;
  if (filename.empty ())
    {
      // We found the symbol table.
      if (!this->read_header (next_off, &filename, &size, NULL, NULL))
	filename.clear ();
    }
  if (filename == "/")
    {
      char *rdbuf = new char[size];
      if (::read (this->fd_, rdbuf, size) != size)
	{
	  rust_error_at (this->location_, "%s: could not read extended names",
			 filename.c_str ());
	  delete[] rdbuf;
	  return false;
	}
      this->extended_names_.assign (rdbuf, size);
      delete[] rdbuf;
    }

  return true;
}

// Read bytes from the file.

bool
Archive_file::read (off_t offset, off_t size, char *buf)
{
  if (::lseek (this->fd_, offset, SEEK_SET) < 0
      || ::read (this->fd_, buf, size) != size)
    {
      rust_error_at (this->location_, "%s: %m", this->filename_.c_str ());
      return false;
    }
  return true;
}

// Parse a decimal in readable text.

bool
Archive_file::parse_decimal (const char *str, off_t size, long *res) const
{
  char *buf = new char[size + 1];
  memcpy (buf, str, size);
  char *ps = buf + size;
  while (ps > buf && ps[-1] == ' ')
    --ps;
  *ps = '\0';

  errno = 0;
  char *end;
  *res = strtol (buf, &end, 10);
  if (*end != '\0' || *res < 0 || (*res == LONG_MAX && errno == ERANGE))
    {
      delete[] buf;
      return false;
    }
  delete[] buf;
  return true;
}

// Read the header at OFF.  Set *PNAME to the name, *SIZE to the size,
// *NESTED_OFF to the nested offset, and *NEXT_OFF to the next member offset.

bool
Archive_file::read_header (off_t off, std::string *pname, off_t *size,
			   off_t *nested_off, off_t *next_off)
{
  if (::lseek (this->fd_, off, SEEK_SET) < 0)
    {
      rust_error_at (this->location_, "%s: %m", this->filename_.c_str ());
      return false;
    }
  if (this->is_big_archive_)
    return this->read_big_archive_header (off, pname, size, next_off);
  else
    return this->read_archive_header (off, pname, size, nested_off, next_off);
}

// Read the big archive header at OFF, setting *PNAME, *SIZE and *NEXT_OFF.

bool
Archive_file::read_big_archive_header (off_t off, std::string *pname,
				       off_t *size, off_t *next_off)
{
  Archive_big_header hdr;
  ssize_t got;

  got = ::read (this->fd_, &hdr, sizeof hdr);
  if (got != sizeof hdr)
    {
      if (got < 0)
	rust_error_at (this->location_, "%s: %m", this->filename_.c_str ());
      else if (got > 0)
	rust_error_at (this->location_, "%s: short entry header at %ld",
		       this->filename_.c_str (), static_cast<long> (off));
      else
	rust_error_at (this->location_, "%s: unexpected EOF at %ld",
		       this->filename_.c_str (), static_cast<long> (off));
    }

  long local_size;
  if (!this->parse_decimal (hdr.ar_size, sizeof (hdr.ar_size), &local_size))
    {
      char *buf = new char[sizeof (hdr.ar_size) + 1];
      memcpy (buf, hdr.ar_size, sizeof (hdr.ar_size));
      rust_error_at (this->location_,
		     ("%s: malformed size in entry header at %ld"
		      " (expected decimal, got %s)"),
		     this->filename_.c_str (), static_cast<long> (off), buf);
      delete[] buf;
      return false;
    }
  *size = local_size;

  long namlen;
  if (!this->parse_decimal (hdr.ar_namlen, sizeof (hdr.ar_namlen), &namlen))
    {
      char *buf = new char[sizeof (hdr.ar_namlen) + 1];
      memcpy (buf, hdr.ar_namlen, sizeof (hdr.ar_namlen));
      rust_error_at (this->location_,
		     ("%s: malformed name length in entry header at %ld"
		      " (expected decimal, got %s)"),
		     this->filename_.c_str (), static_cast<long> (off), buf);
      delete[] buf;
      return false;
    }
  // Read member name following member header.
  char *rdbuf = new char[namlen];
  got = ::read (this->fd_, rdbuf, namlen);
  if (got != namlen)
    {
      rust_error_at (this->location_,
		     "%s: malformed member name in entry header at %ld",
		     this->filename_.c_str (), static_cast<long> (off));
      delete[] rdbuf;
      return false;
    }
  pname->assign (rdbuf, namlen);
  delete[] rdbuf;

  long local_next_off;
  if (!this->parse_decimal (hdr.ar_nxtmem, sizeof (hdr.ar_nxtmem),
			    &local_next_off))
    {
      char *buf = new char[sizeof (hdr.ar_nxtmem) + 1];
      memcpy (buf, hdr.ar_nxtmem, sizeof (hdr.ar_nxtmem));
      rust_error_at (this->location_,
		     ("%s: malformed next member offset in entry header at %ld"
		      " (expected decimal, got %s)"),
		     this->filename_.c_str (), static_cast<long> (off), buf);
      delete[] buf;
      return false;
    }
  if (next_off != NULL)
    {
      if (local_next_off == 0) // Last member.
	*next_off = this->filesize_;
      else
	*next_off = local_next_off;
    }
  return true;
}

// Read the normal archive header at OFF, setting *PNAME, *SIZE,
// *NESTED_OFF and *NEXT_OFF.

bool
Archive_file::read_archive_header (off_t off, std::string *pname, off_t *size,
				   off_t *nested_off, off_t *next_off)
{
  Archive_header hdr;
  ssize_t got = ::read (this->fd_, &hdr, sizeof hdr);
  if (got != sizeof hdr)
    {
      if (got < 0)
	rust_error_at (this->location_, "%s: %m", this->filename_.c_str ());
      else if (got > 0)
	rust_error_at (this->location_, "%s: short archive header at %ld",
		       this->filename_.c_str (), static_cast<long> (off));
      else
	rust_error_at (this->location_, "%s: unexpected EOF at %ld",
		       this->filename_.c_str (), static_cast<long> (off));
    }
  off_t local_nested_off;
  if (!this->interpret_header (&hdr, off, pname, size, &local_nested_off))
    return false;
  if (nested_off != NULL)
    *nested_off = local_nested_off;

  off_t local_next_off;
  local_next_off = off + sizeof (Archive_header);
  if (!this->is_thin_archive_ || pname->empty () || *pname == "/")
    local_next_off += *size;
  if ((local_next_off & 1) != 0)
    ++local_next_off;
  if (local_next_off > this->filesize_) // Last member.
    local_next_off = this->filesize_;
  if (next_off != NULL)
    *next_off = local_next_off;
  return true;
}

// Interpret the header of HDR, the header of the archive member at
// file offset OFF.

bool
Archive_file::interpret_header (const Archive_header *hdr, off_t off,
				std::string *pname, off_t *size,
				off_t *nested_off) const
{
  if (memcmp (hdr->ar_fmag, arfmag, sizeof arfmag) != 0)
    {
      rust_error_at (this->location_, "%s: malformed archive header at %lu",
		     this->filename_.c_str (),
		     static_cast<unsigned long> (off));
      return false;
    }

  long local_size;
  if (!this->parse_decimal (hdr->ar_size, sizeof hdr->ar_size, &local_size))
    {
      rust_error_at (this->location_,
		     "%s: malformed archive header size at %lu",
		     this->filename_.c_str (),
		     static_cast<unsigned long> (off));
      return false;
    }
  *size = local_size;

  *nested_off = 0;
  if (hdr->ar_name[0] != '/')
    {
      const char *name_end = strchr (hdr->ar_name, '/');
      if (name_end == NULL
	  || name_end - hdr->ar_name >= static_cast<int> (sizeof hdr->ar_name))
	{
	  rust_error_at (this->location_,
			 "%s: malformed archive header name at %lu",
			 this->filename_.c_str (),
			 static_cast<unsigned long> (off));
	  return false;
	}
      pname->assign (hdr->ar_name, name_end - hdr->ar_name);
    }
  else if (hdr->ar_name[1] == ' ')
    {
      // This is the symbol table.
      pname->clear ();
    }
  else if (hdr->ar_name[1] == 'S' && hdr->ar_name[2] == 'Y'
	   && hdr->ar_name[3] == 'M' && hdr->ar_name[4] == '6'
	   && hdr->ar_name[5] == '4' && hdr->ar_name[6] == '/'
	   && hdr->ar_name[7] == ' ')
    {
      // 64-bit symbol table.
      pname->clear ();
    }
  else if (hdr->ar_name[1] == '/')
    {
      // This is the extended name table.
      pname->assign (1, '/');
    }
  else
    {
      char *end;
      errno = 0;
      long x = strtol (hdr->ar_name + 1, &end, 10);
      long y = 0;
      if (*end == ':')
	y = strtol (end + 1, &end, 10);
      if (*end != ' ' || x < 0 || (x == LONG_MAX && errno == ERANGE)
	  || static_cast<size_t> (x) >= this->extended_names_.size ())
	{
	  rust_error_at (this->location_, "%s: bad extended name index at %lu",
			 this->filename_.c_str (),
			 static_cast<unsigned long> (off));
	  return false;
	}

      const char *name = this->extended_names_.data () + x;
      const char *name_end = strchr (name, '\n');
      if (static_cast<size_t> (name_end - name) > this->extended_names_.size ()
	  || name_end[-1] != '/')
	{
	  rust_error_at (this->location_,
			 "%s: bad extended name entry at header %lu",
			 this->filename_.c_str (),
			 static_cast<unsigned long> (off));
	  return false;
	}
      pname->assign (name, name_end - 1 - name);
      *nested_off = y;
    }

  return true;
}

// Get the file and offset for an archive member.

bool
Archive_file::get_file_and_offset (off_t off, const std::string &hdrname,
				   off_t nested_off, int *memfd, off_t *memoff,
				   std::string *memname)
{
  if (this->is_big_archive_)
    {
      *memfd = this->fd_;
      *memoff = (off + sizeof (Archive_big_header) + hdrname.length ()
		 + sizeof (arfmag));
      if ((*memoff & 1) != 0)
	++*memoff;
      *memname = this->filename_ + '(' + hdrname + ')';
      return true;
    }
  else if (!this->is_thin_archive_)
    {
      *memfd = this->fd_;
      *memoff = off + sizeof (Archive_header);
      *memname = this->filename_ + '(' + hdrname + ')';
      return true;
    }

  std::string filename = hdrname;
  if (!IS_ABSOLUTE_PATH (filename.c_str ()))
    {
      const char *archive_path = this->filename_.c_str ();
      const char *basename = lbasename (archive_path);
      if (basename > archive_path)
	filename.replace (0, 0,
			  this->filename_.substr (0, basename - archive_path));
    }

  if (nested_off > 0)
    {
      // This is a member of a nested archive.
      Archive_file *nfile;
      Nested_archive_table::const_iterator p
	= this->nested_archives_.find (filename);
      if (p != this->nested_archives_.end ())
	nfile = p->second;
      else
	{
	  int nfd = open (filename.c_str (), O_RDONLY | O_BINARY);
	  if (nfd < 0)
	    {
	      rust_error_at (this->location_,
			     "%s: cannot open nested archive %s",
			     this->filename_.c_str (), filename.c_str ());
	      return false;
	    }
	  nfile = new Archive_file (filename, nfd, this->location_);
	  if (!nfile->initialize ())
	    {
	      delete nfile;
	      return false;
	    }
	  this->nested_archives_[filename] = nfile;
	}

      std::string nname;
      off_t nsize;
      off_t nnested_off;
      if (!nfile->read_header (nested_off, &nname, &nsize, &nnested_off, NULL))
	return false;
      return nfile->get_file_and_offset (nested_off, nname, nnested_off, memfd,
					 memoff, memname);
    }

  // An external member of a thin archive.
  *memfd = open (filename.c_str (), O_RDONLY | O_BINARY);
  if (*memfd < 0)
    {
      rust_error_at (this->location_, "%s: %m", filename.c_str ());
      return false;
    }
  *memoff = 0;
  *memname = filename;
  return true;
}

// An archive member iterator.  This is more-or-less copied from gold.

class Archive_iterator
{
public:
  // The header of an archive member.  This is what this iterator
  // points to.
  struct Header
  {
    // The name of the member.
    std::string name;
    // The file offset of the member.
    off_t off;
    // The file offset of a nested archive member.
    off_t nested_off;
    // The size of the member.
    off_t size;
  };

  Archive_iterator (Archive_file *afile, off_t off) : afile_ (afile), off_ (off)
  {
    this->read_next_header ();
  }

  const Header &operator* () const { return this->header_; }

  const Header *operator-> () const { return &this->header_; }

  Archive_iterator &operator++ ()
  {
    if (this->off_ == this->afile_->filesize ())
      return *this;
    this->off_ = this->next_off_;
    this->read_next_header ();
    return *this;
  }

  Archive_iterator operator++ (int)
  {
    Archive_iterator ret = *this;
    ++*this;
    return ret;
  }

  bool operator== (const Archive_iterator &p) const
  {
    return this->off_ == p->off;
  }

  bool operator!= (const Archive_iterator &p) const
  {
    return this->off_ != p->off;
  }

private:
  void read_next_header ();

  // The underlying archive file.
  Archive_file *afile_;
  // The current offset in the file.
  off_t off_;
  // The offset of the next member.
  off_t next_off_;
  // The current archive header.
  Header header_;
};

// Read the next archive header.

void
Archive_iterator::read_next_header ()
{
  off_t filesize = this->afile_->filesize ();
  while (true)
    {
      if (this->off_ == filesize)
	{
	  this->header_.off = filesize;
	  return;
	}

      if (!this->afile_->read_header (this->off_, &this->header_.name,
				      &this->header_.size,
				      &this->header_.nested_off,
				      &this->next_off_))
	{
	  this->header_.off = filesize;
	  this->off_ = filesize;
	  return;
	}
      this->header_.off = this->off_;

      // Skip special members.
      if (!this->header_.name.empty () && this->header_.name != "/")
	return;

      this->off_ = this->next_off_;
    }
}

// Initial iterator.

Archive_iterator
archive_begin (Archive_file *afile)
{
  return Archive_iterator (afile, afile->first_member_offset ());
}

// Final iterator.

Archive_iterator
archive_end (Archive_file *afile)
{
  return Archive_iterator (afile, afile->filesize ());
}

// A type of Import_stream which concatenates other Import_streams
// together.

class Stream_concatenate : public Import::Stream
{
public:
  Stream_concatenate () : inputs_ () {}

  // Add a new stream.
  void add (std::unique_ptr<Import::Stream> is)
  {
    this->inputs_.push_back (std::move (is));
  }

protected:
  bool do_peek (size_t, const char **);

  void do_advance (size_t);

private:
  std::list<std::unique_ptr<Import::Stream>> inputs_;
};

// Peek ahead.

bool
Stream_concatenate::do_peek (size_t length, const char **bytes)
{
  while (true)
    {
      if (this->inputs_.empty ())
	return false;
      if (this->inputs_.front ()->peek (length, bytes))
	return true;
      this->inputs_.pop_front ();
    }
}

// Advance.

void
Stream_concatenate::do_advance (size_t skip)
{
  while (true)
    {
      if (this->inputs_.empty ())
	return;
      if (!this->inputs_.front ()->at_eof ())
	{
	  // We just assume that this will do the right thing.  It
	  // should be OK since we should never want to skip past
	  // multiple streams.
	  this->inputs_.front ()->advance (skip);
	  return;
	}
      this->inputs_.pop_front ();
    }
}

// Import data from an archive.  We walk through the archive and
// import data from each member.

std::unique_ptr<Import::Stream>
Import::find_archive_export_data (const std::string &filename, int fd,
				  location_t location)
{
  Archive_file afile (filename, fd, location);
  if (!afile.initialize ())
    return nullptr;

  auto ret = std::make_unique<Stream_concatenate> ();

  bool any_data = false;
  bool any_members = false;
  Archive_iterator pend = archive_end (&afile);
  for (Archive_iterator p = archive_begin (&afile); p != pend; p++)
    {
      any_members = true;
      int member_fd;
      off_t member_off;
      std::string member_name;
      if (!afile.get_file_and_offset (p->off, p->name, p->nested_off,
				      &member_fd, &member_off, &member_name))
	return nullptr;

      std::unique_ptr<Import::Stream> is
	= Import::find_object_export_data (member_name, member_fd, member_off,
					   location);
      if (is != nullptr)
	{
	  ret->add (std::move (is));
	  any_data = true;
	}
    }

  if (!any_members)
    {
      // It's normal to have an empty archive file when using gobuild.
      return std::make_unique<Stream_from_string> ("");
    }

  if (!any_data)
    {
      return nullptr;
    }

  return std::unique_ptr<Stream>{static_cast<Stream *> (ret.release ())};
}

} // namespace Rust
