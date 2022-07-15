// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef RUST_IMPORTS_H
#define RUST_IMPORTS_H

#include "rust-system.h"
#include "rust-location.h"

namespace Rust {

extern void
add_search_path (const std::string &path);

class Import
{
public:
  // The Stream class is an interface used to read the data.  The
  // caller should instantiate a child of this class.
  class Stream
  {
  public:
    Stream ();
    virtual ~Stream ();

    // Set the position, for error messages.
    void set_pos (int pos) { this->pos_ = pos; }

    // Return whether we have seen an error.
    bool saw_error () const { return this->saw_error_; }

    // Record that we've seen an error.
    void set_saw_error () { this->saw_error_ = true; }

    // Return the next character (a value from 0 to 0xff) without
    // advancing.  Returns -1 at end of stream.
    int peek_char ();

    // Look for LENGTH characters, setting *BYTES to point to them.
    // Returns false if the bytes are not available.  Does not
    // advance.
    bool peek (size_t length, const char **bytes)
    {
      return this->do_peek (length, bytes);
    }

    // Return the next character (a value from 0 to 0xff) and advance
    // the read position by 1.  Returns -1 at end of stream.
    int get_char ()
    {
      int c = this->peek_char ();
      this->advance (1);
      return c;
    }

    // Return true if at the end of the stream.
    bool at_eof () { return this->peek_char () == -1; }

    // Return true if the next bytes match STR.
    bool match_c_string (const char *str)
    {
      return this->match_bytes (str, strlen (str));
    }

    // Return true if the next LENGTH bytes match BYTES.
    bool match_bytes (const char *bytes, size_t length);

    // Give an error if the next bytes do not match STR.  Advance the
    // read position by the length of STR.
    void require_c_string (Location location, const char *str)
    {
      this->require_bytes (location, str, strlen (str));
    }

    // Given an error if the next LENGTH bytes do not match BYTES.
    // Advance the read position by LENGTH.
    void require_bytes (Location, const char *bytes, size_t length);

    // Advance the read position by SKIP bytes.
    void advance (size_t skip)
    {
      this->do_advance (skip);
      this->pos_ += skip;
    }

    // Return the current read position.  This returns int because it
    // is more convenient in error reporting.  FIXME.
    int pos () { return static_cast<int> (this->pos_); }

    // This function should set *BYTES to point to a buffer holding
    // the LENGTH bytes at the current read position.  It should
    // return false if the bytes are not available.  This should not
    // change the current read position.
    virtual bool do_peek (size_t length, const char **bytes) = 0;

    // This function should advance the current read position LENGTH
    // bytes.
    virtual void do_advance (size_t skip) = 0;

  private:
    // The current read position.
    size_t pos_;
    // True if we've seen an error reading from this stream.
    bool saw_error_;
  };

  // Find import data.  This searches the file system for FILENAME and
  // returns a pointer to a Stream object to read the data that it
  // exports.  LOCATION is the location of the import statement.
  // RELATIVE_IMPORT_PATH is used as a prefix for a relative import.
  static Stream *open_package (const std::string &filename, Location location,
			       const std::string &relative_import_path);

  // Constructor.
  Import (Stream *, Location);

  // The location of the import statement.
  Location location () const { return this->location_; }

  // Return the next character.
  int peek_char () { return this->stream_->peek_char (); }

  // Return the next character and advance.
  int get_char () { return this->stream_->get_char (); }

  // Read LENGTH characters into *OUT and advance past them.  On
  // EOF reports an error and sets *OUT to an empty string.
  void read (size_t length, std::string *out);

  // Return true at the end of the stream.
  bool at_eof () { return this->stream_->at_eof (); }

  // Return whether the next bytes match STR.
  bool match_c_string (const char *str)
  {
    return this->stream_->match_c_string (str);
  }

  // Require that the next bytes match STR.
  void require_c_string (const char *str)
  {
    this->stream_->require_c_string (this->location_, str);
  }

  // Advance the stream SKIP bytes.
  void advance (size_t skip) { this->stream_->advance (skip); }

  // Stream position, for error reporting.
  int pos () { return this->stream_->pos (); }

  // Clear the stream when it is no longer accessible.
  void clear_stream () { this->stream_ = NULL; }

private:
  static Stream *try_package_in_directory (const std::string &, Location);

  static int try_suffixes (std::string *);

  static Stream *find_export_data (const std::string &filename, int fd,
				   Location);

  static Stream *find_object_export_data (const std::string &filename, int fd,
					  off_t offset, Location);

  static bool is_archive_magic (const char *);

  static Stream *find_archive_export_data (const std::string &filename, int fd,
					   Location);

  // The stream from which to read import data.
  Stream *stream_;
  // The location of the import statement we are processing.
  Location location_;
};

// Read import data from a string.

class Stream_from_string : public Import::Stream
{
public:
  Stream_from_string (const std::string &str) : str_ (str), pos_ (0) {}

  bool do_peek (size_t length, const char **bytes)
  {
    if (this->pos_ + length > this->str_.length ())
      return false;
    *bytes = this->str_.data () + this->pos_;
    return true;
  }

  void do_advance (size_t len) { this->pos_ += len; }

private:
  // The string of data we are reading.
  std::string str_;
  // The current position within the string.
  size_t pos_;
};

// Read import data from a buffer allocated using malloc.

class Stream_from_buffer : public Import::Stream
{
public:
  Stream_from_buffer (char *buf, size_t length)
    : buf_ (buf), length_ (length), pos_ (0)
  {}

  ~Stream_from_buffer () { free (this->buf_); }

  bool do_peek (size_t length, const char **bytes)
  {
    if (this->pos_ + length > this->length_)
      return false;
    *bytes = this->buf_ + this->pos_;
    return true;
  }

  void do_advance (size_t len) { this->pos_ += len; }

private:
  // The data we are reading.
  char *buf_;
  // The length of the buffer.
  size_t length_;
  // The current position within the buffer.
  size_t pos_;
};

// Read import data from an open file descriptor.

class Stream_from_file : public Import::Stream
{
public:
  Stream_from_file (int fd);

  ~Stream_from_file ();

  bool do_peek (size_t, const char **);

  void do_advance (size_t);

private:
  // No copying.
  Stream_from_file (const Stream_from_file &);
  Stream_from_file &operator= (const Stream_from_file &);

  // The file descriptor.
  int fd_;
  // Data read from the file.
  std::string data_;
};

} // namespace Rust

#endif // RUST_IMPORTS_H
