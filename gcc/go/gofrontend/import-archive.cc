// import-archive.cc -- Go frontend read import data from an archive file.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include "import.h"

#ifndef O_BINARY
#define O_BINARY 0
#endif

// Archive magic numbers.

static const char armag[] =
{
  '!', '<', 'a', 'r', 'c', 'h', '>', '\n'
};

static const char armagt[] =
{
  '!', '<', 't', 'h', 'i', 'n', '>', '\n'
};

static const char arfmag[2] = { '`', '\n' };

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

// The functions in this file extract Go export data from an archive.

const int Import::archive_magic_len;

// Return true if BYTES, which are from the start of the file, are an
// archive magic number.

bool
Import::is_archive_magic(const char* bytes)
{
  return (memcmp(bytes, armag, Import::archive_magic_len) == 0
	  || memcmp(bytes, armagt, Import::archive_magic_len) == 0);
}

// An object used to read an archive file.

class Archive_file
{
 public:
  Archive_file(const std::string& filename, int fd, source_location location)
    : filename_(filename), fd_(fd), filesize_(-1), extended_names_(),
      is_thin_archive_(false), location_(location), nested_archives_()
  { }

  // Initialize.
  bool
  initialize();

  // Return the file name.
  const std::string&
  filename() const
  { return this->filename_; }

  // Get the file size.
  off_t
  filesize() const
  { return this->filesize_; }

  // Return whether this is a thin archive.
  bool
  is_thin_archive() const
  { return this->is_thin_archive_; }

  // Return the location of the import statement.
  source_location
  location() const
  { return this->location_; }

  // Read bytes.
  bool
  read(off_t offset, off_t size, char*);

  // Read the archive header at OFF, setting *PNAME, *SIZE, and
  // *NESTED_OFF.
  bool
  read_header(off_t off, std::string* pname, off_t* size, off_t* nested_off);

  // Interpret the header of HDR, the header of the archive member at
  // file offset OFF.  Return whether it succeeded.  Set *SIZE to the
  // size of the member.  Set *PNAME to the name of the member.  Set
  // *NESTED_OFF to the offset in a nested archive.
  bool
  interpret_header(const Archive_header* hdr, off_t off,
		   std::string* pname, off_t* size, off_t* nested_off) const;

  // Get the file and offset for an archive member.
  bool
  get_file_and_offset(off_t off, const std::string& hdrname,
		      off_t nested_off, int* memfd, off_t* memoff,
		      std::string* memname);

 private:
  // For keeping track of open nested archives in a thin archive file.
  typedef std::map<std::string, Archive_file*> Nested_archive_table;

  // The name of the file.
  std::string filename_;
  // The file descriptor.
  int fd_;
  // The file size;
  off_t filesize_;
  // The extended name table.
  std::string extended_names_;
  // Whether this is a thin archive.
  bool is_thin_archive_;
  // The location of the import statements.
  source_location location_;
  // Table of nested archives.
  Nested_archive_table nested_archives_;
};

bool
Archive_file::initialize()
{
  struct stat st;
  if (fstat(this->fd_, &st) < 0)
    {
      error_at(this->location_, "%s: %m", this->filename_.c_str());
      return false;
    }
  this->filesize_ = st.st_size;

  char buf[sizeof(armagt)];
  if (::lseek(this->fd_, 0, SEEK_SET) < 0
      || ::read(this->fd_, buf, sizeof(armagt)) != sizeof(armagt))
    {
      error_at(this->location_, "%s: %m", this->filename_.c_str());
      return false;
    }
  this->is_thin_archive_ = memcmp(buf, armagt, sizeof(armagt)) == 0;

  if (this->filesize_ == sizeof(armag))
    {
      // Empty archive.
      return true;
    }

  // Look for the extended name table.
  std::string filename;
  off_t size;
  if (!this->read_header(sizeof(armagt), &filename, &size, NULL))
    return false;
  if (filename.empty())
    {
      // We found the symbol table.
      off_t off = sizeof(armagt) + sizeof(Archive_header) + size;
      if ((off & 1) != 0)
	++off;
      if (!this->read_header(off, &filename, &size, NULL))
	filename.clear();
    }
  if (filename == "/")
    {
      char* rdbuf = new char[size];
      if (::read(this->fd_, rdbuf, size) != size)
	{
	  error_at(this->location_, "%s: could not read extended names",
		   filename.c_str());
	  delete[] rdbuf;
	  return false;
	}
      this->extended_names_.assign(rdbuf, size);
      delete[] rdbuf;
    }

  return true;
}

// Read bytes from the file.

bool
Archive_file::read(off_t offset, off_t size, char* buf)
{
  if (::lseek(this->fd_, offset, SEEK_SET) < 0
      || ::read(this->fd_, buf, size) != size)
    {
      error_at(this->location_, "%s: %m", this->filename_.c_str());
      return false;
    }
  return true;
}

// Read the header at OFF.  Set *PNAME to the name, *SIZE to the size,
// and *NESTED_OFF to the nested offset.

bool
Archive_file::read_header(off_t off, std::string* pname, off_t* size,
			  off_t* nested_off)
{
  Archive_header hdr;
  if (::lseek(this->fd_, off, SEEK_SET) < 0)
    {
      error_at(this->location_, "%s: %m", this->filename_.c_str());
      return false;
    }
  ssize_t got = ::read(this->fd_, &hdr, sizeof hdr);
  if (got != sizeof hdr)
    {
      if (got < 0)
	error_at(this->location_, "%s: %m", this->filename_.c_str());
      else if (got > 0)
	error_at(this->location_, "%s: short archive header at %ld",
		 this->filename_.c_str(), static_cast<long>(off));
      else
	error_at(this->location_, "%s: unexpected EOF at %ld",
		 this->filename_.c_str(), static_cast<long>(off));
    }
  off_t local_nested_off;
  if (!this->interpret_header(&hdr, off, pname, size, &local_nested_off))
    return false;
  if (nested_off != NULL)
    *nested_off = local_nested_off;
  return true;
}

// Interpret the header of HDR, the header of the archive member at
// file offset OFF.

bool
Archive_file::interpret_header(const Archive_header* hdr, off_t off,
			       std::string* pname, off_t* size,
			       off_t* nested_off) const
{
  if (memcmp(hdr->ar_fmag, arfmag, sizeof arfmag) != 0)
    {
      error_at(this->location_, "%s: malformed archive header at %lu",
	       this->filename_.c_str(), static_cast<unsigned long>(off));
      return false;
    }

  const int size_string_size = sizeof hdr->ar_size;
  char size_string[size_string_size + 1];
  memcpy(size_string, hdr->ar_size, size_string_size);
  char* ps = size_string + size_string_size;
  while (ps[-1] == ' ')
    --ps;
  *ps = '\0';

  errno = 0;
  char* end;
  *size = strtol(size_string, &end, 10);
  if (*end != '\0'
      || *size < 0
      || (*size == LONG_MAX && errno == ERANGE))
    {
      error_at(this->location_, "%s: malformed archive header size at %lu",
	       this->filename_.c_str(), static_cast<unsigned long>(off));
      return false;
    }

  if (hdr->ar_name[0] != '/')
    {
      const char* name_end = strchr(hdr->ar_name, '/');
      if (name_end == NULL
	  || name_end - hdr->ar_name >= static_cast<int>(sizeof hdr->ar_name))
	{
	  error_at(this->location_, "%s: malformed archive header name at %lu",
		   this->filename_.c_str(), static_cast<unsigned long>(off));
	  return false;
	}
      pname->assign(hdr->ar_name, name_end - hdr->ar_name);
      *nested_off = 0;
    }
  else if (hdr->ar_name[1] == ' ')
    {
      // This is the symbol table.
      pname->clear();
    }
  else if (hdr->ar_name[1] == '/')
    {
      // This is the extended name table.
      pname->assign(1, '/');
    }
  else
    {
      errno = 0;
      long x = strtol(hdr->ar_name + 1, &end, 10);
      long y = 0;
      if (*end == ':')
        y = strtol(end + 1, &end, 10);
      if (*end != ' '
	  || x < 0
	  || (x == LONG_MAX && errno == ERANGE)
	  || static_cast<size_t>(x) >= this->extended_names_.size())
	{
	  error_at(this->location_, "%s: bad extended name index at %lu",
		   this->filename_.c_str(), static_cast<unsigned long>(off));
	  return false;
	}

      const char* name = this->extended_names_.data() + x;
      const char* name_end = strchr(name, '\n');
      if (static_cast<size_t>(name_end - name) > this->extended_names_.size()
	  || name_end[-1] != '/')
	{
	  error_at(this->location_, "%s: bad extended name entry at header %lu",
		   this->filename_.c_str(), static_cast<unsigned long>(off));
	  return false;
	}
      pname->assign(name, name_end - 1 - name);
      if (nested_off != NULL)
        *nested_off = y;
    }

  return true;
}

// Get the file and offset for an archive member.

bool
Archive_file::get_file_and_offset(off_t off, const std::string& hdrname,
				  off_t nested_off, int* memfd, off_t* memoff,
				  std::string* memname)
{
  if (!this->is_thin_archive_)
    {
      *memfd = this->fd_;
      *memoff = off + sizeof(Archive_header);
      *memname = this->filename_ + '(' + hdrname + ')';
      return true;
    }

  std::string filename = hdrname;
  if (!IS_ABSOLUTE_PATH(filename.c_str()))
    {
      const char* archive_path = this->filename_.c_str();
      const char* basename = lbasename(archive_path);
      if (basename > archive_path)
	filename.replace(0, 0,
			 this->filename_.substr(0, basename - archive_path));
    }

  if (nested_off > 0)
    {
      // This is a member of a nested archive.
      Archive_file* nfile;
      Nested_archive_table::const_iterator p =
	this->nested_archives_.find(filename);
      if (p != this->nested_archives_.end())
	nfile = p->second;
      else
	{
	  int nfd = open(filename.c_str(), O_RDONLY | O_BINARY);
	  if (nfd < 0)
	    {
	      error_at(this->location_, "%s: can't open nested archive %s",
		       this->filename_.c_str(), filename.c_str());
	      return false;
	    }
	  nfile = new Archive_file(filename, nfd, this->location_);
	  if (!nfile->initialize())
	    {
	      delete nfile;
	      return false;
	    }
	  this->nested_archives_[filename] = nfile;
	}

      std::string nname;
      off_t nsize;
      off_t nnested_off;
      if (!nfile->read_header(nested_off, &nname, &nsize, &nnested_off))
	return false;
      return nfile->get_file_and_offset(nested_off, nname, nnested_off,
					memfd, memoff, memname);
    }

  // An external member of a thin archive.
  *memfd = open(filename.c_str(), O_RDONLY | O_BINARY);
  if (*memfd < 0)
    {
      error_at(this->location_, "%s: %m", filename.c_str());
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

  Archive_iterator(Archive_file* afile, off_t off)
    : afile_(afile), off_(off)
  { this->read_next_header(); }

  const Header&
  operator*() const
  { return this->header_; }

  const Header*
  operator->() const
  { return &this->header_; }

  Archive_iterator&
  operator++()
  {
    if (this->off_ == this->afile_->filesize())
      return *this;
    this->off_ += sizeof(Archive_header);
    if (!this->afile_->is_thin_archive())
      this->off_ += this->header_.size;
    if ((this->off_ & 1) != 0)
      ++this->off_;
    this->read_next_header();
    return *this;
  }

  Archive_iterator
  operator++(int)
  {
    Archive_iterator ret = *this;
    ++*this;
    return ret;
  }

  bool
  operator==(const Archive_iterator p) const
  { return this->off_ == p->off; }

  bool
  operator!=(const Archive_iterator p) const
  { return this->off_ != p->off; }

 private:
  void
  read_next_header();

  // The underlying archive file.
  Archive_file* afile_;
  // The current offset in the file.
  off_t off_;
  // The current archive header.
  Header header_;
};

// Read the next archive header.

void
Archive_iterator::read_next_header()
{
  off_t filesize = this->afile_->filesize();
  while (true)
    {
      if (filesize - this->off_ < static_cast<off_t>(sizeof(Archive_header)))
	{
	  if (filesize != this->off_)
	    {
	      error_at(this->afile_->location(),
		       "%s: short archive header at %lu",
		       this->afile_->filename().c_str(),
		       static_cast<unsigned long>(this->off_));
	      this->off_ = filesize;
	    }
	  this->header_.off = filesize;
	  return;
	}

      char buf[sizeof(Archive_header)];
      if (!this->afile_->read(this->off_, sizeof(Archive_header), buf))
	{
	  this->header_.off = filesize;
	  return;
	}

      const Archive_header* hdr = reinterpret_cast<const Archive_header*>(buf);
      if (!this->afile_->interpret_header(hdr, this->off_, &this->header_.name,
					  &this->header_.size,
					  &this->header_.nested_off))
	{
	  this->header_.off = filesize;
	  return;
	}
      this->header_.off = this->off_;

      // Skip special members.
      if (!this->header_.name.empty() && this->header_.name != "/")
	return;

      this->off_ += sizeof(Archive_header) + this->header_.size;
      if ((this->off_ & 1) != 0)
	++this->off_;
    }
}

// Initial iterator.

Archive_iterator
archive_begin(Archive_file* afile)
{
  return Archive_iterator(afile, sizeof(armag));
}

// Final iterator.

Archive_iterator
archive_end(Archive_file* afile)
{
  return Archive_iterator(afile, afile->filesize());
}

// A type of Import_stream which concatenates other Import_streams
// together.

class Stream_concatenate : public Import::Stream
{
 public:
  Stream_concatenate()
    : inputs_()
  { }

  // Add a new stream.
  void
  add(Import::Stream* is)
  { this->inputs_.push_back(is); }

 protected:
  bool
  do_peek(size_t, const char**);

  void
  do_advance(size_t);

 private:
  std::list<Import::Stream*> inputs_;
};

// Peek ahead.

bool
Stream_concatenate::do_peek(size_t length, const char** bytes)
{
  while (true)
    {
      if (this->inputs_.empty())
	return false;
      if (this->inputs_.front()->peek(length, bytes))
	return true;
      delete this->inputs_.front();
      this->inputs_.pop_front();
    }
}

// Advance.

void
Stream_concatenate::do_advance(size_t skip)
{
  while (true)
    {
      if (this->inputs_.empty())
	return;
      if (!this->inputs_.front()->at_eof())
	{
	  // We just assume that this will do the right thing.  It
	  // should be OK since we should never want to skip past
	  // multiple streams.
	  this->inputs_.front()->advance(skip);
	  return;
	}
      delete this->inputs_.front();
      this->inputs_.pop_front();
    }
}

// Import data from an archive.  We walk through the archive and
// import data from each member.

Import::Stream*
Import::find_archive_export_data(const std::string& filename, int fd,
				 source_location location)
{
  Archive_file afile(filename, fd, location);
  if (!afile.initialize())
    return NULL;

  Stream_concatenate* ret = new Stream_concatenate;

  bool any_data = false;
  bool any_members = false;
  Archive_iterator pend = archive_end(&afile);
  for (Archive_iterator p = archive_begin(&afile); p != pend; p++)
    {
      any_members = true;
      int member_fd;
      off_t member_off;
      std::string member_name;
      if (!afile.get_file_and_offset(p->off, p->name, p->nested_off,
				     &member_fd, &member_off, &member_name))
	return NULL;

      Import::Stream* is = Import::find_object_export_data(member_name,
							   member_fd,
							   member_off,
							   location);
      if (is != NULL)
	{
	  ret->add(is);
	  any_data = true;
	}
    }

  if (!any_members)
    {
      // It's normal to have an empty archive file when using gobuild.
      return new Stream_from_string("");
    }

  if (!any_data)
    {
      delete ret;
      return NULL;
    }

  return ret;
}
