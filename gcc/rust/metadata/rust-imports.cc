// Copyright (C) 2020-2025 Free Software Foundation, Inc.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include "rust-system.h"
#include "rust-diagnostics.h"
#include "rust-imports.h"
#include "rust-object-export.h"
#include "rust-export-metadata.h"

#ifndef O_BINARY
#define O_BINARY 0
#endif

namespace Rust {

// The list of paths we search for import files.
static std::vector<std::string> search_path;

// Add a directory to the search path.  This is called from the option
// handling language hook.
void
add_search_path (const std::string &path)
{
  search_path.push_back (path);
}

// Find import data.  This searches the file system for FILENAME and
// returns a pointer to a Stream object to read the data that it
// exports.  If the file is not found, it returns NULL.

// When FILENAME is not an absolute path and does not start with ./ or
// ../, we use the search path provided by -I and -L options.

// When FILENAME does start with ./ or ../, we use
// RELATIVE_IMPORT_PATH as a prefix.

// When FILENAME does not exist, we try modifying FILENAME to find the
// file.  We use the first of these which exists:
//   * We append ".gox".
//   * We turn the base of FILENAME into libFILENAME.so.
//   * We turn the base of FILENAME into libFILENAME.a.
//   * We append ".o".

// When using a search path, we apply each of these transformations at
// each entry on the search path before moving on to the next entry.
// If the file exists, but does not contain any Rust export data, we
// stop; we do not keep looking for another file with the same name
// later in the search path.

std::pair<std::unique_ptr<Import::Stream>, std::vector<ProcMacro::Procmacro>>
Import::open_package (const std::string &filename, location_t location,
		      const std::string &relative_import_path)
{
  bool is_local;
  if (IS_ABSOLUTE_PATH (filename))
    is_local = true;
  else if (filename[0] == '.'
	   && (filename[1] == '\0' || IS_DIR_SEPARATOR (filename[1])))
    is_local = true;
  else if (filename[0] == '.' && filename[1] == '.'
	   && (filename[2] == '\0' || IS_DIR_SEPARATOR (filename[2])))
    is_local = true;
  else
    is_local = false;

  std::string fn = filename;
  if (is_local && !IS_ABSOLUTE_PATH (filename)
      && !relative_import_path.empty ())
    {
      if (fn == ".")
	{
	  // A special case.
	  fn = relative_import_path;
	}
      else if (fn[0] == '.' && fn[1] == '.'
	       && (fn[2] == '\0' || IS_DIR_SEPARATOR (fn[2])))
	{
	  // We are going to join relative_import_path and fn, and it
	  // will look like DIR/../PATH.  But DIR does not necessarily
	  // exist in this case, and if it doesn't the use of .. will
	  // fail although it shouldn't.  The gc compiler uses
	  // path.Join here, which cleans up the .., so we need to do
	  // the same.
	  size_t index;
	  for (index = relative_import_path.length () - 1;
	       index > 0 && !IS_DIR_SEPARATOR (relative_import_path[index]);
	       index--)
	    ;
	  if (index > 0)
	    fn = relative_import_path.substr (0, index) + fn.substr (2);
	  else
	    fn = relative_import_path + '/' + fn;
	}
      else
	fn = relative_import_path + '/' + fn;
      is_local = false;
    }

  if (!is_local)
    {
      for (std::vector<std::string>::const_iterator p = search_path.begin ();
	   p != search_path.end (); ++p)
	{
	  std::string indir = *p;
	  if (!indir.empty () && indir[indir.size () - 1] != '/')
	    indir += '/';
	  indir += fn;
	  auto s = Import::try_package_in_directory (indir, location);
	  if (s.first != nullptr)
	    return s;
	}
    }

  auto s = Import::try_package_in_directory (fn, location);
  if (s.first != nullptr)
    return s;

  return std::make_pair (nullptr, std::vector<ProcMacro::Procmacro>{});
}

// Try to find the export data for FILENAME.

std::pair<std::unique_ptr<Import::Stream>, std::vector<ProcMacro::Procmacro>>
Import::try_package_in_directory (const std::string &filename,
				  location_t location)
{
  std::string found_filename = filename;
  int fd = open (found_filename.c_str (), O_RDONLY | O_BINARY);

  if (fd >= 0)
    {
      struct stat s;
      if (fstat (fd, &s) >= 0 && S_ISDIR (s.st_mode))
	{
	  close (fd);
	  fd = -1;
	  errno = EISDIR;
	}
    }

  if (fd < 0)
    {
      if (errno != ENOENT && errno != EISDIR)
	rust_warning_at (location, 0, "%s: %m", filename.c_str ());

      fd = Import::try_suffixes (&found_filename);
      if (fd < 0)
	return std::make_pair (nullptr, std::vector<ProcMacro::Procmacro>{});
    }

  auto macros = load_macros (found_filename);

  // The export data may not be in this file.
  std::unique_ptr<Stream> s
    = Import::find_export_data (found_filename, fd, location);
  if (s != nullptr)
    return std::make_pair (std::move (s), macros);

  close (fd);

  if (macros.empty ())
    rust_error_at (location,
		   "%s exists but does not contain any Rust export data",
		   found_filename.c_str ());

  return std::make_pair (nullptr, macros);
}

// Given import "*PFILENAME", where *PFILENAME does not exist, try
// various suffixes.  If we find one, set *PFILENAME to the one we
// found.  Return the open file descriptor.

int
Import::try_suffixes (std::string *pfilename)
{
  std::string filename = *pfilename + ".rox";
  int fd = open (filename.c_str (), O_RDONLY | O_BINARY);
  if (fd >= 0)
    {
      *pfilename = filename;
      return fd;
    }

  const char *basename = lbasename (pfilename->c_str ());
  size_t basename_pos = basename - pfilename->c_str ();
  filename = pfilename->substr (0, basename_pos) + "lib" + basename + ".so";
  fd = open (filename.c_str (), O_RDONLY | O_BINARY);
  if (fd >= 0)
    {
      *pfilename = filename;
      return fd;
    }

  filename = pfilename->substr (0, basename_pos) + "lib" + basename + ".a";
  fd = open (filename.c_str (), O_RDONLY | O_BINARY);
  if (fd >= 0)
    {
      *pfilename = filename;
      return fd;
    }

  filename = *pfilename + ".o";
  fd = open (filename.c_str (), O_RDONLY | O_BINARY);
  if (fd >= 0)
    {
      *pfilename = filename;
      return fd;
    }

  return -1;
}

// Look for export data in the file descriptor FD.

std::unique_ptr<Import::Stream>
Import::find_export_data (const std::string &filename, int fd,
			  location_t location)
{
  // See if we can read this as an object file.
  std::unique_ptr<Import::Stream> stream
    = Import::find_object_export_data (filename, fd, 0, location);
  if (stream != nullptr)
    return stream;

  const int len = sizeof (Metadata::kMagicHeader);
  if (lseek (fd, 0, SEEK_SET) < 0)
    {
      rust_error_at (location, "lseek %s failed: %m", filename.c_str ());
      return nullptr;
    }

  char buf[len];
  ssize_t c = ::read (fd, buf, len);
  if (c < len)
    return nullptr;

  // Check for a file containing nothing but Rust export data.
  // if (memcmp (buf, Export::cur_magic, Export::magic_len) == 0
  //     || memcmp (buf, Export::v1_magic, Export::magic_len) == 0
  //     || memcmp (buf, Export::v2_magic, Export::magic_len) == 0)
  //
  // FIXME we need to work out a better header
  //
  if (memcmp (buf, Metadata::kMagicHeader, sizeof (Metadata::kMagicHeader))
      == 0)
    return std::make_unique<Stream_from_file> (fd);

  // See if we can read this as an archive.
  if (Import::is_archive_magic (buf))
    return Import::find_archive_export_data (filename, fd, location);

  return nullptr;
}

// Look for export data in an object file.

std::unique_ptr<Import::Stream>
Import::find_object_export_data (const std::string &filename, int fd,
				 off_t offset, location_t location)
{
  char *buf;
  size_t len;
  int err;
  const char *errmsg = rust_read_export_data (fd, offset, &buf, &len, &err);
  if (errmsg != nullptr)
    {
      if (err == 0)
	rust_error_at (location, "%s: %s", filename.c_str (), errmsg);
      else
	rust_error_at (location, "%s: %s: %s", filename.c_str (), errmsg,
		       xstrerror (err));
      return nullptr;
    }

  if (buf == nullptr)
    return nullptr;

  return std::make_unique<Stream_from_buffer> (buf, len);
}

// Class Import.

// Construct an Import object.  We make the builtin_types_ vector
// large enough to hold all the builtin types.

Import::Import (std::unique_ptr<Stream> stream, location_t location)
  : stream_ (std::move (stream)), location_ (location)
{}

// Import the data in the associated stream.

// Read LENGTH bytes from the stream.

void
Import::read (size_t length, std::string *out)
{
  const char *data;
  if (!this->stream_->peek (length, &data))
    {
      if (!this->stream_->saw_error ())
	rust_error_at (this->location_, "import error at %d: expected %d bytes",
		       this->stream_->pos (), static_cast<int> (length));
      this->stream_->set_saw_error ();
      *out = std::string ("");
      return;
    }
  *out = std::string (data, length);
  this->advance (length);
}

// Class Import::Stream.

Import::Stream::Stream () : pos_ (0), saw_error_ (false) {}

Import::Stream::~Stream () {}

// Return the next character to come from the stream.

int
Import::Stream::peek_char ()
{
  const char *read;
  if (!this->do_peek (1, &read))
    return -1;
  // Make sure we return an unsigned char, so that we don't get
  // confused by \xff.
  unsigned char ret = *read;
  return ret;
}

// Return true if the next LENGTH characters from the stream match
// BYTES

bool
Import::Stream::match_bytes (const char *bytes, size_t length)
{
  const char *read;
  if (!this->do_peek (length, &read))
    return false;
  return memcmp (bytes, read, length) == 0;
}

// Require that the next LENGTH bytes from the stream match BYTES.

void
Import::Stream::require_bytes (location_t location, const char *bytes,
			       size_t length)
{
  const char *read;
  if (!this->do_peek (length, &read) || memcmp (bytes, read, length) != 0)
    {
      if (!this->saw_error_)
	rust_error_at (location, "import error at %d: expected %<%.*s%>",
		       this->pos (), static_cast<int> (length), bytes);
      this->saw_error_ = true;
      return;
    }
  this->advance (length);
}

// Class Stream_from_file.

Stream_from_file::Stream_from_file (int fd) : fd_ (fd), data_ ()
{
  if (lseek (fd, 0, SEEK_SET) != 0)
    {
      rust_fatal_error (UNKNOWN_LOCATION, "lseek failed: %m");
      this->set_saw_error ();
    }
}

Stream_from_file::~Stream_from_file () { close (this->fd_); }

// Read next bytes.

bool
Stream_from_file::do_peek (size_t length, const char **bytes)
{
  if (this->data_.length () >= length)
    {
      *bytes = this->data_.data ();
      return true;
    }

  this->data_.resize (length);
  ssize_t got = ::read (this->fd_, &this->data_[0], length);

  if (got < 0)
    {
      if (!this->saw_error ())
	rust_fatal_error (UNKNOWN_LOCATION, "read failed: %m");
      this->set_saw_error ();
      return false;
    }

  if (lseek (this->fd_, -got, SEEK_CUR) < 0)
    {
      if (!this->saw_error ())
	rust_fatal_error (UNKNOWN_LOCATION, "lseek failed: %m");
      this->set_saw_error ();
      return false;
    }

  if (static_cast<size_t> (got) < length)
    return false;

  *bytes = this->data_.data ();
  return true;
}

// Advance.

void
Stream_from_file::do_advance (size_t skip)
{
  if (lseek (this->fd_, skip, SEEK_CUR) < 0)
    {
      if (!this->saw_error ())
	rust_fatal_error (UNKNOWN_LOCATION, "lseek failed: %m");
      this->set_saw_error ();
    }
  if (!this->data_.empty ())
    {
      if (this->data_.length () > skip)
	this->data_.erase (0, skip);
      else
	this->data_.clear ();
    }
}

} // namespace Rust
