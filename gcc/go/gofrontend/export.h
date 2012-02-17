// export.h -- Export declarations in Go frontend.     -*- C++ -*-

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GO_EXPORT_H
#define GO_EXPORT_H

#include "string-dump.h"

struct sha1_ctx;
class Gogo;
class Import_init;
class Bindings;
class Type;

// Codes used for the builtin types.  These are all negative to make
// them easily distinct from the codes assigned by Export::write_type.
// Note that these codes may not be changed!  Changing them would
// break existing export data.

enum Builtin_code
{
  BUILTIN_INT8 = -1,
  BUILTIN_INT16 = -2,
  BUILTIN_INT32 = -3,
  BUILTIN_INT64 = -4,
  BUILTIN_UINT8 = -5,
  BUILTIN_UINT16 = -6,
  BUILTIN_UINT32 = -7,
  BUILTIN_UINT64 = -8,
  BUILTIN_FLOAT32 = -9,
  BUILTIN_FLOAT64 = -10,
  BUILTIN_INT = -11,
  BUILTIN_UINT = -12,
  BUILTIN_UINTPTR = -13,
  BUILTIN_BOOL = -15,
  BUILTIN_STRING = -16,
  BUILTIN_COMPLEX64 = -17,
  BUILTIN_COMPLEX128 = -18,
  BUILTIN_ERROR = -19,
  BUILTIN_BYTE = -20,
  BUILTIN_RUNE = -21,

  SMALLEST_BUILTIN_CODE = -21
};

// This class manages exporting Go declarations.  It handles the main
// loop of exporting.  A pointer to this class is also passed to the
// various specific export implementations.

class Export : public String_dump
{
 public:
  // The Stream class is an interface used to output the exported
  // information.  The caller should instantiate a child of this
  // class.
  class Stream
  {
   public:
    Stream();
    virtual ~Stream();

    // Write a string. Implements the String_dump interface.
    void
    write_string(const std::string& s)
    { this->write_and_sum_bytes(s.data(), s.length()); }

    // Write a nul terminated string. Implements the String_dump interface.
    void
    write_c_string(const char* s)
    { this->write_and_sum_bytes(s, strlen(s)); }

    // Write some bytes.
    void
    write_bytes(const char* bytes, size_t length)
    { this->write_and_sum_bytes(bytes, length); }

    // Return the raw bytes of the checksum data.
    std::string
    checksum();

    // Write a checksum string to the stream.  This will be called at
    // the end of the other output.
    void
    write_checksum(const std::string&);

   protected:
    // This function is called with data to export.  This data must be
    // made available as a contiguous stream for the importer.
    virtual void
    do_write(const char* bytes, size_t length) = 0;

  private:
    void
    write_and_sum_bytes(const char*, size_t);

    // The checksum.
    sha1_ctx* checksum_;
  };

  Export(Stream*);

  // The magic code for version 1 export data.
  static const int v1_magic_len = 4;
  static const char v1_magic[v1_magic_len];

  // The length of the v1 checksum string.
  static const int v1_checksum_len = 20;

  // Register the builtin types.
  void
  register_builtin_types(Gogo*);

  // Export the identifiers in BINDINGS which are marked for export.
  // The exporting is done via a series of calls to THIS->STREAM_.  If
  // is nothing to export, this->stream_->write will not be called.
  // UNIQUE_PREFIX is a prefix for all global symbols.
  // PACKAGE_PRIORITY is the priority to use for this package.
  // IMPORT_INIT_FN is the name of the import initialization function
  // for this package; it will be empty if none is needed.
  // IMPORTED_INIT_FNS is the list of initialization functions for
  // imported packages.
  void
  export_globals(const std::string& package_name,
		 const std::string& unique_prefix,
		 int package_priority,
		 const std::string& import_init_fn,
		 const std::set<Import_init>& imported_init_fns,
		 const Bindings* bindings);

  // Write a string to the export stream.
  void
  write_string(const std::string& s)
  { this->stream_->write_string(s); }

  // Write a nul terminated string to the export stream.
  void
  write_c_string(const char* s)
  { this->stream_->write_c_string(s); }

  // Write some bytes to the export stream.
  void
  write_bytes(const char* bytes, size_t length)
  { this->stream_->write_bytes(bytes, length); }

  // Write a name to the export stream.  If NAME is empty, write "?".
  void
  write_name(const std::string& name);

  // Write out a type.  This handles references back to previous
  // definitions.
  void
  write_type(const Type*);

 private:
  Export(const Export&);
  Export& operator=(const Export&);

  // Write out the imported initialization functions.
  void
  write_imported_init_fns(const std::string& package_name, int priority,
			  const std::string&, const std::set<Import_init>&);

  // Register one builtin type.
  void
  register_builtin_type(Gogo*, const char* name, Builtin_code);

  // Mapping from Type objects to a constant index.
  typedef Unordered_map(const Type*, int) Type_refs;

  // The stream to which we are writing data.
  Stream* stream_;
  // Type mappings.
  Type_refs type_refs_;
  // Index number of next type.
  int type_index_;
};

// An export streamer which puts the export stream in a named section.

class Stream_to_section : public Export::Stream
{
 public:
  Stream_to_section();

 protected:
  void
  do_write(const char*, size_t);
};

#endif // !defined(GO_EXPORT_H)
