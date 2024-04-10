// export.h -- Export declarations in Go frontend.     -*- C++ -*-

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GO_EXPORT_H
#define GO_EXPORT_H

#include "string-dump.h"

class Go_sha1_helper;
class Gogo;
class Named_object;
class Export_function_body;
class Import_init;
class Named_object;
class Bindings;
class Type;
class Package;
class Import_init_set;
class Backend;
class Temporary_statement;
class Unnamed_label;
struct Export_impl;

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
  BUILTIN_ANY = -22,

  SMALLEST_BUILTIN_CODE = -22
};

// Export data version number. New export data is written with the
// "current" version, but there is support for reading files with
// older version export data (at least for now).

enum Export_data_version {
  EXPORT_FORMAT_UNKNOWN = 0,
  EXPORT_FORMAT_V1 = 1,
  EXPORT_FORMAT_V2 = 2,
  EXPORT_FORMAT_V3 = 3,
  EXPORT_FORMAT_CURRENT = EXPORT_FORMAT_V3
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

    // The checksum helper.
    Go_sha1_helper* sha1_helper_;
  };

  Export(Stream*);
  ~Export();

  // Size of export data magic string (which includes version number).
  static const int magic_len = 4;

  // Magic strings (current version and older versions).
  static const char cur_magic[magic_len];
  static const char v1_magic[magic_len];
  static const char v2_magic[magic_len];

  // The length of the checksum string.
  static const int checksum_len = 20;

  // Register the builtin types.
  void
  register_builtin_types(Gogo*);

  // Export the identifiers in BINDINGS which are marked for export.
  // The exporting is done via a series of calls to THIS->STREAM_.  If
  // is nothing to export, this->stream_->write will not be called.
  // PREFIX is the package prefix.  PKGPATH is the package path.
  // Only one of PREFIX and PKGPATH will be non-empty.
  // PACKAGES is all the packages we have seen.
  // IMPORTS is the explicitly imported packages.
  // IMPORT_INIT_FN is the name of the import initialization function
  // for this package; it will be empty if none is needed.
  // IMPORTED_INIT_FNS is the list of initialization functions for
  // imported packages.
  void
  export_globals(const std::string& package_name,
		 const std::string& prefix,
		 const std::string& pkgpath,
		 const std::map<std::string, Package*>& packages,
		 const std::map<std::string, Package*>& imports,
		 const std::string& import_init_fn,
		 const Import_init_set& imported_init_fns,
		 const Bindings* bindings,
                 Unordered_set(Named_object*)* marked_inline_functions);

  // Record a type that is mentioned in export data. Return value is
  // TRUE for newly visited types, FALSE for types that have been seen
  // previously.
  bool
  record_type(Type*);

  // Assign type indices to types mentioned in export data.
  int
  assign_type_indices(const std::vector<Named_object*>& sorted_exports);

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

  // Write a type to an exported function body.
  void
  write_type_to(const Type*, Export_function_body*);

  // Write the escape note to the export stream.  If NOTE is NULL, write
  // nothing.
  void
  write_escape(std::string* note);

  // Write an integer value.
  void
  write_int(int);

  // Write an unsigned value.
  void
  write_unsigned(unsigned);

  // Return the index of a package.
  int
  package_index(const Package* p) const;

  // Return the index of the "unsafe" package, which must be one of
  // the exported packages.
  int
  unsafe_package_index() const;

 private:
  Export(const Export&);
  Export& operator=(const Export&);

  // Write out all known packages.
  void
  write_packages(const std::map<std::string, Package*>& packages);

  typedef std::map<unsigned, std::set<unsigned> > Init_graph;

  static void
  add_init_graph_edge(Init_graph* init_graph, unsigned src, unsigned sink);

  static void
  populate_init_graph(Init_graph* init_graph,
                      const Import_init_set& imported_init_fns,
                      const std::map<std::string, unsigned>& init_idx);

  // Write out the imported packages.
  void
  write_imports(const std::map<std::string, Package*>& imports,
		const Unordered_set(const Package*)& type_imports);

  // Write out the imported initialization functions and init graph.
  void
  write_imported_init_fns(const std::string& package_name,
			  const std::string&, const Import_init_set&);

  // Write out all types.
  void
  write_types(int unexported_type_index);

  // Write out one type definition.
  void
  write_type_definition(const Type* type, int index);

  // Register one builtin type.
  void
  register_builtin_type(Gogo*, const char* name, Builtin_code);

  // Return the index of a type in the export data.
  int
  type_index(const Type*);

  // Set the index of a type.
  void
  set_type_index(const Type*);

  // The stream to which we are writing data.
  Stream* stream_;
  // Index number of next type.
  int type_index_;
  // Packages we have written out.
  Unordered_map(const Package*, int) packages_;
  // Hidden implementation-specific state.
  Export_impl* impl_;
};

// An export streamer that puts the export stream in a named section.

class Stream_to_section : public Export::Stream
{
 public:
  Stream_to_section(Backend*);

 protected:
  void
  do_write(const char*, size_t);

 private:
  Backend* backend_;
};

// An export streamer that puts the export stream in a string.

class Stream_to_string : public Export::Stream
{
 public:
  Stream_to_string()
    : string_()
  {}

  const std::string&
  string() const
  { return this->string_; }

 protected:
  void
  do_write(const char* s, size_t len)
  { this->string_.append(s, len); }

 private:
  std::string string_;
};

// Class to manage exporting a function body.  This is passed around
// to Statements and Expressions.  It builds up the export data for
// the function.

class Export_function_body : public String_dump
{
 public:
  Export_function_body(Export* exp, int indent)
    : exp_(exp), body_(), type_context_(NULL), next_temporary_index_(0),
      temporary_indexes_(), next_label_index_(0), label_indexes_(),
      indent_(indent)
  { }

  // Write a character to the body.
  void
  write_char(char c)
  { this->body_.append(1, c); }

  // Write a NUL terminated string to the body.
  void
  write_c_string(const char* str)
  { this->body_.append(str); }

  // Write a string to the body.
  void
  write_string(const std::string& str)
  { this->body_.append(str); }

  // Write a type reference to the body.
  void
  write_type(const Type* type)
  { this->exp_->write_type_to(type, this); }

  // Return the current type context.
  Type*
  type_context() const
  { return this->type_context_; }

  // Set the current type context.
  void
  set_type_context(Type* type)
  { this->type_context_ = type; }

  // Append as many spaces as the current indentation level.
  void
  indent()
  {
    for (int i = this->indent_; i > 0; i--)
      this->write_char(' ');
  }

  // Increment the indentation level.
  void
  increment_indent()
  { ++this->indent_; }

  // Decrement the indentation level.
  void
  decrement_indent()
  { --this->indent_; }

  // Return the index of a package.
  int
  package_index(const Package* p) const
  { return this->exp_->package_index(p); }

  // Return the index of the "unsafe" package.
  int
  unsafe_package_index() const
  { return this->exp_->unsafe_package_index(); }

  // Record a temporary statement and return its index.
  unsigned int
  record_temporary(const Temporary_statement*);

  // Return the index of a temporary statement.
  unsigned int
  temporary_index(const Temporary_statement*);

  // Return the index of an unnamed label.  If it doesn't already have
  // an index, give it one.
  unsigned int
  unnamed_label_index(const Unnamed_label*);

  // Return a reference to the completed body.
  const std::string&
  body() const
  { return this->body_; }

 private:
  // The overall export data.
  Export* exp_;
  // The body we are building.
  std::string body_;
  // Current type context.  Used to avoid duplicate type conversions.
  Type* type_context_;
  // Index to give to next temporary statement.
  unsigned int next_temporary_index_;
  // Map temporary statements to indexes.
  Unordered_map(const Temporary_statement*, unsigned int) temporary_indexes_;
  // Index to give to the next unnamed label.
  unsigned int next_label_index_;
  // Map unnamed labels to indexes.
  Unordered_map(const Unnamed_label*, unsigned int) label_indexes_;
  // Current indentation level: the number of spaces before each statement.
  int indent_;
};

#endif // !defined(GO_EXPORT_H)
