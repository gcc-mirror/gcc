// names.cc -- Names used by gofrontend generated code.

// Copyright 2017 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include "gogo.h"
#include "go-encode-id.h"
#include "types.h"
#include "expressions.h"

// This file contains functions that generate names that appear in the
// assembly code.  This is not used for names that appear only in the
// debug info.

// Our external names may contain only ASCII alphanumeric characters,
// underscore, and dot.  (According to the GCC sources, dot is not
// permitted in assembler symbols on VxWorks and MMIX.  We will not
// support those systems.)  Go identifiers cannot contain dot, but Go
// package paths can.  Both Go identifiers and package paths can, of
// course, contain all sorts of Unicode characters.
//
// The gc compiler uses names like "pkg.F", and it seems convenient to
// emulate that.  Therefore, we will use dot to separate different
// components of names.
//
// Since package paths can contain dot, to avoid ambiguity we must
// encode package paths such that they do not contain any dot.  The
// natural way to do this is to encode forbidden characters, including
// dot, using a notation based on underscore.  We will, of course,
// have to encode underscore itself.
//
// Since we will be using an underscore encoding for the package path,
// it seems reasonable to use the same encoding for Go identifiers.
// This has the disadvantage that encoded Go identifiers will appear
// to be valid Go identifiers with funny spellings, but it seems like
// the best available approach.
//
// Therefore, in the following discussion we may assume that none of
// the names under discussion contain a dot.  All of the names we
// generate for Go identifiers (that don't use //export or
// //go:linkname) will contain at least one dot, as discussed below.
// We assume that none of the non-Go symbols in the final link will
// contain a dot, so we don't worry about conflicts.
//
// We first describe the basic symbol names, used to represent Go
// functions and variables.
//
// The external name for a normal Go symbol NAME, a function or
// variable, is simply "PKGPATH.NAME".  Note that NAME is not the
// packed form used for the "hidden" name internally in the compiler;
// it is the name that appears in the source code.  Both PKGPATH and
// NAME will be encoded as described below.  The encoding process
// ensures that neither encoded string can contain a dot, and neither
// will start with a digit (NAME is a Go identifier that can't contain
// a dot or start with a digit anyhow).  The encoding process means
// that these external names contain exactly one dot and do not start
// with a dot.
//
// The external name for a method NAME for a named type TYPE is
// "PKGPATH.TYPE.NAME".  Both NAME and TYPE are simple Go identifiers.
// Unlike the gc compiler, the external name does not indicate whether
// this is a pointer method or a value method; a named type can not
// have both a pointer and value method with the same name, so there
// is no ambiguity.  PKGPATH is the package path of the package in
// which TYPE is defined.  PKGPATH, TYPE, and NAME are encoded, and
// cannot be empty or contain a dot or start with a digit.  These
// external names contain exactly two dots, not consecutive, and they
// do not start with a dot.
//
// It's uncommon, but the use of type literals with embedded fields
// can cause us to have methods on unnamed types.  The external names
// for these are also PKGPATH.TYPELIT.NAME, where TYPELIT is an
// approximately readable version of the type literal, described
// below.  A TYPELIT will always contain characters that cannot appear
// in a Go identifier, so TYPELIT can never be confused with a TYPE
// name.  There is no ambiguity as long as encoded type literals are
// unambiguous.
//
// Also uncommon is an external name that must refer to a named type
// defined within a function.  While such a type can not have methods
// itself, it can pick up embedded methods, and those methods need
// names.  These are treated as a kind of type literal written as,
// before type literal encoding, FNNAME.TYPENAME(INDEX) or, for a
// method, TYPE.MNAME.TYPENAME(INDEX).  INDEX is the index of that
// named type within the function, as a single function can have
// multiple types with the same name.  This is unambiguous as
// parentheses can not appear in a type literal in this form (they can
// only appear in interface method declarations).
//
// That is the end of the list of basic names.  The remaining names
// exist for special purposes, and are differentiated from the basic
// names by containing two consecutive dots.
//
// The hash function for a type is treated as a method whose name is
// ".hash".  That is, the method name begins with a dot.  The effect
// is that there will be two consecutive dots in the name; the name
// will always end with "..hash".
//
// Similarly the equality function for a type is treated as a method
// whose name is ".eq".
//
// The function descriptor for a function is the same as the name of
// the function with an added suffix "..f".
//
// A thunk for a go or defer statement is treated as a function whose
// name is ".thunkNN", unencoded, where NN is a sequence of digits
// (these functions are never globally visible).  Thus the final name
// of a thunk will be PKGPATH..thunkNN (PKGPATH is encoded).
//
// An init function is treated as a function whose name is ".initNN",
// unencoded, where NN is a sequence of digits (these functions are
// never globally visible).  Thus the final name of an init function
// will be PKGPATH..initNN (PKGPATH is encoded).
//
// A nested function is given the name of outermost enclosing function
// or method with an added suffix "..funcNN", unencoded, where NN is a
// sequence of digits.  Note that the function descriptor of a nested
// function, if needed, will end with "..funcNN..f".
//
// A recover thunk is the same as the name of the function with an
// added suffix "..r".
//
// The name of a type descriptor for a named type is
// PKGPATH.TYPENAME..d (PKGPATH and TYPENAME are encoded).
//
// The name of a type descriptor for a pointer to a named type is
// PKGPATH.TYPENAME..p (PKGPATH and TYPENAME are encoded).
//
// The name of a type descriptor for an unnamed type is type..TYPELIT.
// That is, the string "type.." followed by the encoded type literal.
// These names are common symbols, in the linker's sense of the word
// common: in the final executable there is only one instance of the
// type descriptor for a given unnamed type.
//
// The name of the GC symbol for a named type is PKGPATH.TYPE..g
// (PKGPATH and TYPE are encoded).
//
// The name of the GC symbol for an unnamed type is type..TYPELIT..g.
// These are common symbols.
//
// The name of a ptrmask symbol is gcbits..B32 where B32 is an
// encoding of the ptrmask bits using only ASCII letters.  These are
// common symbols.
//
// An interface method table for assigning the non-interface type TYPE
// to the interface type ITYPE is named imt..ITYPE..TYPE.  If ITYPE or
// TYPE is a named type, they are written as PKGPATH.TYPE (where both
// PKGPATH and TYPE are encoded).  Otherwise they are written as a
// type literal.  An interface method table for a pointer method set
// uses pimt instead of imt.
//
// The names of composite literal initializers, including the GC root
// variable, are not referenced.  They must not conflict with any C
// language names, but the names are otherwise unimportant.  They are
// named "go..CNN" where NN is a sequence of digits.  The names do not
// include the PKGPATH.
//
// The map zero value, a common symbol that represents the zero value
// of a map, is named simply "go..zerovalue".  The name does not
// include the PKGPATH.
//
// The import function for the main package is referenced by C code,
// and is named __go_init_main.  For other packages it is
// PKGPATH..import.  If a package doesn't need an init function, it
// will have a dummy one, named ~PKGPATH.
//
// In each package there is a list of all the type descriptors defined
// in this package.  The name of the list is PKGPATH..types.
//
// In the main package it gathers all the type descriptor lists in a
// single list, named go..typelists.
//
// The type literal encoding is essentially a single line version of
// the type literal, such as "struct { pkgpath.i int; J int }".  In
// this representation unexported names use their pkgpath, exported
// names omit it.
//
// The type literal encoding is not quite valid Go, as some aspects of
// compiler generated types can not be represented.  For example,
// incomparable struct types have an extra field "{x}".  Struct tags
// can contain any character, which will be underscore encoded as
// usual.  In the unusual case of a curly brace or a backslash in a
// struct tag, the brace or backslash will be backslash quoted, before
// underscore encoding.
//
// Many of these names will be visible in the debugger.  The debugger
// will be given these names before applying any underscore encoding.
// These user names do not have to be unique--they are only used by
// the debugger, not the linker--so this is OK.  However, there is an
// exception: if the name would otherwise include characters that
// can't normally appear in an identifier, then the user name will
// also be underscore encoded.  This avoids problems with
// communicating the debug info to the assembler and with handling the
// debug info in the debugger.  A Go-aware debugger will need to know
// whether to apply underscore decoding to a name before showing it to
// the user.  We indicate this by adding a prefix of "g.", and
// assuming that cases of a package path of "g" are unusual.  This
// prefix will only appear in the user name, not the assembler name.
//
// The underscore encoding is, naturally, an underscore followed by
// other characters.  As there are various characters that commonly
// appear in type literals and in package paths, we have a set of
// short encodings.  Then we have general encodings for other
// characters.
//
//   __ - '_'
//   _0 - '.'
//   _1 - '/'
//   _2 - '*'
//   _3 - ','
//   _4 - '{'
//   _5 - '}'
//   _6 - '['
//   _7 - ']'
//   _8 - '('
//   _9 - ')'
//   _a - '"'
//   _b - ' '
//   _c - ';'
//
// Other non-alphanumeric ASCII characters are encoded as _xNN, where
// NN is the hex value for the character.  If an encoded name would
// otherwise start with a digit, this encoding is also used for the
// leading digit.
//
// Non-ASCII Unicode characters are encoded as _u and four hex digits
// or _U and eight digits, just as in the language only using _u and
// _U instead of \u and \U.
//
// Demangling these names is straightforward:
//  - replace _xXX with an ASCII character
//  - replace _uXXXX with a unicode character
//  - replace _UXXXXXXXX with a unicode character
//  - replace _C per the table above
// That will get you as close as possible to a readable name.

// Set BNAME to the name to use for an exported function, a method, or
// a function/method declaration.  GO_NAME is the name that appears in
// the Go code.  PACKAGE is the package where the function is defined,
// and is NULL for the package being compiled.  For a method, RTYPE is
// the method's receiver type; for a function, RTYPE is NULL.

void
Gogo::function_backend_name(const std::string& go_name,
			    const Package* package, const Type* rtype,
			    Backend_name* bname)
{
  if (rtype != NULL)
    rtype->deref()->backend_name(this, bname);
  else if (package == NULL)
    bname->add(this->pkgpath());
  else
    bname->add(package->pkgpath());

  size_t pos = Gogo::special_name_pos(go_name);
  if (pos == std::string::npos)
    bname->add(Gogo::unpack_hidden_name(go_name));
  else
    {
      if (pos > 0)
	bname->add(go_name.substr(0, pos));
      bname->set_suffix(go_name.substr(pos));
    }
}

// Set BNAME to the name to use for a function descriptor.  These
// symbols are globally visible.

void
Gogo::function_descriptor_backend_name(Named_object* no,
				       Backend_name* bname)
{
  if (no->is_function())
    no->func_value()->backend_name(this, no, bname);
  else if (no->is_function_declaration())
    no->func_declaration_value()->backend_name(this, no, bname);
  else
    go_unreachable();
  bname->append_suffix("..f");
}

// Return the name to use for a generated stub method.  A stub method
// is used as the method table entry for a promoted method of an
// embedded type.  MNAME is the method name.  PACKAGE is the package
// where the type that needs this stub method is defined.  These
// functions are globally visible.
//
// This returns a name that acts like a Go identifier, as though the
// stub method were written in Go as an explicitly defined method that
// simply calls the promoted method.  The name we return here will
// eventually be passed to function_backend_name, which will return a
// name that includes the receiver type.
//
// We construct a unique method name and append "..stub".
// function_backend_name will look for the "..stub" and turn that into
// an unencoded suffix.  The rest of the name will be encoded as
// usual.

std::string
Gogo::stub_method_name(const Package* package, const std::string& mname)
{
  if (!Gogo::is_hidden_name(mname))
    return mname + "..stub";

  const std::string& ppkgpath(package == NULL
			      ? this->pkgpath()
			      : package->pkgpath());
  std::string mpkgpath = Gogo::hidden_name_pkgpath(mname);
  if (mpkgpath == ppkgpath)
    return Gogo::unpack_hidden_name(mname) + "..stub";

  // We are creating a stub method for an unexported method of an
  // imported embedded type.  A single type can have multiple promoted
  // methods with the same unexported name, if it embeds types from
  // different packages.  We need to disambiguate the method name.
  // This produces an unambiguous name because even though MPKGPATH
  // can be anything, we know that MNAME does not contain a dot.  The
  // dot we return here, between MPKGPATH and MNAME, will wind up
  // being underscore encoded.
  std::string ret(mpkgpath);
  ret.push_back('.');
  ret.append(Gogo::unpack_hidden_name(mname));
  ret.append("..stub");
  return ret;
}

// Set BNAME to the name of the hash function for TYPE.

void
Gogo::hash_function_name(const Type* type, Backend_name* bname)
{
  if (type->named_type() != NULL)
    type->backend_name(this, bname);
  else
    {
      bname->add(this->pkgpath());
      type->backend_name(this, bname);
    }
  bname->set_suffix("..hash");
}

// Set BNAME to the name of the equal function for TYPE.  If NAME is
// not NULL it is the name of the type.

void
Gogo::equal_function_name(const Type* type, const Named_type* name,
			  Backend_name* bname)
{
  if (name != NULL)
    name->backend_name(this, bname);
  else
    {
      bname->add(this->pkgpath());
      type->backend_name(this, bname);
    }
  bname->set_suffix("..eq");
}

// Set BNAME to the name to use for a global variable.  GO_NAME is the
// name that appears in the Go code.  PACKAGE is the package where the
// variable is defined, and is NULL for the package being compiled.

void
Gogo::global_var_backend_name(const std::string& go_name,
			      const Package* package,
			      Backend_name* bname)
{
  if (package == NULL)
    bname->add(this->pkgpath());
  else
    bname->add(package->pkgpath());
  bname->add(Gogo::unpack_hidden_name(go_name));
}

// Return an erroneous name that indicates that an error has already
// been reported.  This name will act like a Go identifier.

std::string
Gogo::erroneous_name()
{
  go_assert(saw_errors());
  static int erroneous_count;
  char name[50];
  snprintf(name, sizeof name, ".erroneous%d", erroneous_count);
  ++erroneous_count;
  return name;
}

// Return whether a name is an erroneous name.

bool
Gogo::is_erroneous_name(const std::string& name)
{
  return name.compare(0, 10, ".erroneous") == 0;
}

// Return a name for a thunk object.  This name will act like a Go
// identifier.  The name returned here will eventually be passed to
// function_backend_name, which will pull off the ..thunk as an
// unencoded suffix.

std::string
Gogo::thunk_name()
{
  static int thunk_count;
  char thunk_name[50];
  snprintf(thunk_name, sizeof thunk_name, "..thunk%d", thunk_count);
  ++thunk_count;
  // We don't want to return a name that starts with a dot, as that
  // will confuse Gogo::is_hidden_name.  And we don't want to change
  // ..thunk, which fits our general theme and is used by code like
  // runtime.Callers.  But the prefix doesn't matter, as the actual
  // name will include the package path.
  std::string ret = "go";
  return ret + thunk_name;
}

// Return whether a function is a thunk.

bool
Gogo::is_thunk(const Named_object* no)
{
  const std::string& name(no->name());
  size_t i = name.rfind("..thunk");
  if (i == std::string::npos)
    return false;
  return Gogo::is_digits(name.substr(i + 7));
}

// Return the name to use for an init function.  There can be multiple
// functions named "init" so each one needs a different name.

std::string
Gogo::init_function_name()
{
  static int init_count;
  char buf[30];
  snprintf(buf, sizeof buf, "..init%d", init_count);
  ++init_count;
  return this->pkgpath() + buf;
}

// Return the name to use for a nested function.  This name acts like
// a Go identifier.  This name will be rewritten by
// Function::backend_name.

std::string
Gogo::nested_function_name(Named_object* enclosing)
{
  std::string prefix;
  unsigned int index;
  if (enclosing == NULL)
    {
      // A function literal at top level, as in
      // var f = func() {}
      static unsigned int toplevel_index;
      ++toplevel_index;
      index = toplevel_index;
      prefix = ".go";
    }
  else
    {
      while (true)
	{
	  Named_object* parent = enclosing->func_value()->enclosing();
	  if (parent == NULL)
	    break;
	  enclosing = parent;
	}
      const Typed_identifier* rcvr =
	enclosing->func_value()->type()->receiver();
      if (rcvr != NULL)
	{
	  Backend_name bname;
	  rcvr->type()->backend_name(this, &bname);
	  prefix = bname.name();
	  prefix.push_back('.');
	}
      prefix.append(Gogo::unpack_hidden_name(enclosing->name()));
      index = enclosing->func_value()->next_nested_function_index();
    }
  char buf[30];
  snprintf(buf, sizeof buf, "..func%u", index);
  return prefix + buf;
}

// Return the name to use for a sink function, a function whose name
// is simply underscore.  We don't really need these functions but we
// do have to generate them for error checking.

std::string
Gogo::sink_function_name()
{
  static int sink_count;
  char buf[30];
  snprintf(buf, sizeof buf, ".sink%d", sink_count);
  ++sink_count;
  return buf;
}

// Return the name to use for a redefined function.  These functions
// are erroneous but we still generate them for further error
// checking.

std::string
Gogo::redefined_function_name()
{
  static int redefinition_count;
  char buf[30];
  snprintf(buf, sizeof buf, ".redefined%d", redefinition_count);
  ++redefinition_count;
  return buf;
}

// Return the name to use for a recover thunk for the function NAME.
// If the function is a method, RTYPE is the receiver type.  This is a
// name that acts like a Go identifier.

std::string
Gogo::recover_thunk_name(const std::string& name, const Type* rtype)
{
  std::string ret;
  if (rtype != NULL)
    {
      Backend_name bname;
      rtype->deref()->backend_name(this, &bname);
      ret = bname.name();
      ret.append(1, '.');
    }
  if (Gogo::special_name_pos(name) != std::string::npos)
    ret.append(name);
  else
    ret.append(Gogo::unpack_hidden_name(name));
  ret.append("..r");
  return ret;
}

// Return the name to use for a GC root variable.  The GC root
// variable is a composite literal that is passed to
// runtime.registerGCRoots.  There is at most one of these variables
// per compilation.

std::string
Gogo::gc_root_name()
{
  return "go..C0";
}

// Return the name to use for a composite literal or string
// initializer.  This is a local name never referenced outside of this
// file.

std::string
Gogo::initializer_name()
{
  static unsigned int counter;
  char buf[30];
  ++counter;
  snprintf(buf, sizeof buf, "go..C%u", counter);
  return buf;
}

// Return the assembler name of the variable used to represent the
// zero value of a map.  This is a globally visible common symbol.

std::string
Gogo::map_zero_value_name()
{
  return "go..zerovalue";
}

// Return the name to use for the import control function.  This name
// is handled specially by Function::backend_name.  It is not encoded
// further.

const std::string&
Gogo::get_init_fn_name()
{
  if (this->init_fn_name_.empty())
    {
      go_assert(this->package_ != NULL);
      if (this->is_main_package())
	{
	  // Use a name that the runtime knows.
	  this->init_fn_name_ = "__go_init_main";
	}
      else
	{
	  std::string s = this->pkgpath_symbol();
	  s.append("..import");
	  this->init_fn_name_ = s;
	}
    }

  return this->init_fn_name_;
}

// Return the name for a dummy init function, which is not a real
// function but only for tracking transitive import.

std::string
Gogo::dummy_init_fn_name()
{
  return "~" + this->pkgpath_symbol();
}

// Return the package path symbol from an init function name, which
// can be a real init function or a dummy one.

std::string
Gogo::pkgpath_symbol_from_init_fn_name(std::string name)
{
  go_assert(!name.empty());
  if (name[0] == '~')
    return name.substr(1);
  size_t pos = name.find("..import");
  if (pos != std::string::npos)
    return name.substr(0, pos);
  go_unreachable();
}

// Set BNAME to a name for a type to use in a symbol.  Return a name
// for a type to use in a symbol.  These names appear in symbol names
// in the assembler file for things like type descriptors and methods.

void
Type::backend_name(Gogo* gogo, Backend_name* bname) const
{
  // Special case top level named types to get nicer name encodings
  // for this common case.
  const Named_type* nt = this->unalias()->named_type();
  if (nt != NULL && !nt->is_builtin())
    {
      unsigned int index;
      if (nt->in_function(&index) == NULL)
	{
	  const Named_object* no = nt->named_object();
	  if (no->package() == NULL)
	    bname->add(gogo->pkgpath());
	  else
	    bname->add(no->package()->pkgpath());
	  bname->add(Gogo::unpack_hidden_name(no->name()));
	  return;
	}
    }

  std::string name;
  bool is_non_identifier = false;

  // The do_symbol_name virtual function will set RET to the mangled
  // name before encoding.
  this->do_mangled_name(gogo, &name, &is_non_identifier);

  bname->add(name);
  if (is_non_identifier)
    bname->set_is_non_identifier();
}

// The mangled name is implemented as a method on each instance of
// Type.

void
Error_type::do_mangled_name(Gogo*, std::string* ret,
			    bool* is_non_identifier) const
{
  ret->append("{error}");
  *is_non_identifier = true;
}

void
Void_type::do_mangled_name(Gogo*, std::string* ret,
			   bool* is_non_identifier) const
{
  ret->append("{void}");
  *is_non_identifier = true;
}

void
Boolean_type::do_mangled_name(Gogo*, std::string* ret, bool*) const
{
  ret->append("bool");
}

void
Integer_type::do_mangled_name(Gogo*, std::string* ret,
			      bool* is_non_identifier) const
{
  char buf[100];
  snprintf(buf, sizeof buf, "%s%si%d",
	   this->is_abstract_ ? "{abstract}" : "",
	   this->is_unsigned_ ? "u" : "",
	   this->bits_);
  ret->append(buf);
  if (this->is_abstract_)
    *is_non_identifier = true;
}

void
Float_type::do_mangled_name(Gogo*, std::string* ret,
			    bool* is_non_identifier) const
{
  char buf[100];
  snprintf(buf, sizeof buf, "%sfloat%d",
	   this->is_abstract_ ? "{abstract}" : "",
	   this->bits_);
  ret->append(buf);
  if (this->is_abstract_)
    *is_non_identifier = true;
}

void
Complex_type::do_mangled_name(Gogo*, std::string* ret,
			      bool* is_non_identifier) const
{
  char buf[100];
  snprintf(buf, sizeof buf, "%sc%d",
	   this->is_abstract_ ? "{abstract}" : "",
	   this->bits_);
  ret->append(buf);
  if (this->is_abstract_)
    *is_non_identifier = true;
}

void
String_type::do_mangled_name(Gogo*, std::string* ret, bool*) const
{
  ret->append("string");
}

void
Function_type::do_mangled_name(Gogo* gogo, std::string* ret,
			       bool* is_non_identifier) const
{
  ret->append("func");

  if (this->receiver_ != NULL)
    {
      ret->push_back('(');
      this->append_mangled_name(this->receiver_->type(), gogo, ret,
				is_non_identifier);
      ret->append(")");
    }

  ret->push_back('(');
  const Typed_identifier_list* params = this->parameters();
  if (params != NULL)
    {
      bool first = true;
      for (Typed_identifier_list::const_iterator p = params->begin();
	   p != params->end();
	   ++p)
	{
	  if (first)
	    first = false;
	  else
	    ret->push_back(',');
	  if (this->is_varargs_ && p + 1 == params->end())
	    ret->append("...");
	  this->append_mangled_name(p->type(), gogo, ret,
				    is_non_identifier);
	}
    }
  ret->push_back(')');

  ret->push_back('(');
  const Typed_identifier_list* results = this->results();
  if (results != NULL)
    {
      bool first = true;
      for (Typed_identifier_list::const_iterator p = results->begin();
	   p != results->end();
	   ++p)
	{
	  if (first)
	    first = false;
	  else
	    ret->append(",");
	  this->append_mangled_name(p->type(), gogo, ret, is_non_identifier);
	}
    }
  ret->push_back(')');

  *is_non_identifier = true;
}

void
Pointer_type::do_mangled_name(Gogo* gogo, std::string* ret,
			      bool* is_non_identifier) const
{
  ret->push_back('*');
  this->append_mangled_name(this->to_type_, gogo, ret, is_non_identifier);
  *is_non_identifier = true;
}

void
Nil_type::do_mangled_name(Gogo*, std::string* ret,
			  bool* is_non_identifier) const
{
  ret->append("{nil}");
  *is_non_identifier = true;
}

void
Struct_type::do_mangled_name(Gogo* gogo, std::string* ret,
			     bool* is_non_identifier) const
{
  ret->append("struct{");

  if (this->is_struct_incomparable_)
    ret->append("{x}");

  const Struct_field_list* fields = this->fields_;
  if (fields != NULL)
    {
      bool first = true;
      for (Struct_field_list::const_iterator p = fields->begin();
	   p != fields->end();
	   ++p)
	{
	  if (first)
	    first = false;
	  else
	    ret->push_back(';');

	  if (!p->is_anonymous())
	    {
              Gogo::append_possibly_hidden_name(ret, p->field_name());
	      ret->push_back(' ');
	    }

	  const Type* ft = p->type();
	  const Named_type* nt = ft->named_type();

	  if (p->is_anonymous() && nt != NULL && nt->is_builtin())
	    {
	      // For an embedded field with a builtin type, we must
	      // include a package path.  Otherwise embedding builtin
	      // types in different packages will produce identical
	      // types, which shouldn't happen because the builtin
	      // types are not exported.
	      ret->append(gogo->pkgpath());
	      ret->push_back('.');
	      nt->append_symbol_type_name(gogo, true, ret, is_non_identifier);
	    }
	  else if (p->is_anonymous() && nt != NULL && nt->is_alias())
	    {
	      // For an anonymous field with an alias type, the field name
	      // is the alias name.
	      nt->append_symbol_type_name(gogo, true, ret, is_non_identifier);
	    }
	  else
	    this->append_mangled_name(ft, gogo, ret, is_non_identifier);

	  if (p->has_tag())
	    {
	      // Use curly braces around a struct tag, since they are
	      // unambiguous here and struct tags rarely contain curly
	      // braces.
	      ret->push_back('{');
	      ret->append(go_mangle_struct_tag(p->tag()));
	      ret->push_back('}');
	    }
	}
    }

  ret->push_back('}');

  *is_non_identifier = true;
}

void
Array_type::do_mangled_name(Gogo* gogo, std::string* ret,
			    bool* is_non_identifier) const
{
  ret->push_back('[');
  if (this->length_ != NULL)
    {
      Numeric_constant nc;
      if (!this->length_->numeric_constant_value(&nc))
	{
	  go_assert(saw_errors());
	  return;
	}
      mpz_t val;
      if (!nc.to_int(&val))
	{
	  go_assert(saw_errors());
	  return;
	}
      char *s = mpz_get_str(NULL, 10, val);
      ret->append(s);
      free(s);
      mpz_clear(val);
      if (this->is_array_incomparable_)
	ret->append("x");
    }
  ret->push_back(']');
  this->append_mangled_name(this->element_type_, gogo, ret, is_non_identifier);
  *is_non_identifier = true;
}

void
Map_type::do_mangled_name(Gogo* gogo, std::string* ret,
			  bool* is_non_identifier) const
{
  ret->append("map[");
  this->append_mangled_name(this->key_type_, gogo, ret, is_non_identifier);
  ret->push_back(']');
  this->append_mangled_name(this->val_type_, gogo, ret, is_non_identifier);
  *is_non_identifier = true;
}

void
Channel_type::do_mangled_name(Gogo* gogo, std::string* ret,
			      bool* is_non_identifier) const
{
  if (!this->may_send_)
    ret->append("<-");
  ret->append("chan");
  if (!this->may_receive_)
    ret->append("<-");
  ret->push_back(' ');
  this->append_mangled_name(this->element_type_, gogo, ret, is_non_identifier);
  *is_non_identifier = true;
}

void
Interface_type::do_mangled_name(Gogo* gogo, std::string* ret,
				bool* is_non_identifier) const
{
  go_assert(this->methods_are_finalized_);

  ret->append("interface{");

  const Typed_identifier_list* methods = this->all_methods_;
  if (methods != NULL && !this->seen_)
    {
      this->seen_ = true;
      bool first = true;
      for (Typed_identifier_list::const_iterator p = methods->begin();
	   p != methods->end();
	   ++p)
	{
	  if (first)
	    first = false;
	  else
	    ret->push_back(';');

	  if (!p->name().empty())
	    {
              Gogo::append_possibly_hidden_name(ret, p->name());
	      ret->push_back(' ');
	    }

	  this->append_mangled_name(p->type(), gogo, ret, is_non_identifier);
	}
      this->seen_ = false;
    }

  ret->push_back('}');

  *is_non_identifier = true;
}

void
Named_type::do_mangled_name(Gogo* gogo, std::string* ret,
			    bool* is_non_identifier) const
{
  this->append_symbol_type_name(gogo, false, ret, is_non_identifier);
}

void
Forward_declaration_type::do_mangled_name(Gogo* gogo, std::string* ret,
					  bool *is_non_identifier) const
{
  if (this->is_defined())
    this->append_mangled_name(this->real_type(), gogo, ret, is_non_identifier);
  else
    {
      const Named_object* no = this->named_object();
      if (no->package() == NULL)
	ret->append(gogo->pkgpath());
      else
	ret->append(no->package()->pkgpath());
      ret->push_back('.');
      ret->append(Gogo::unpack_hidden_name(no->name()));
    }
}

// Append the symbol name for a named type to RET.  For an alias we
// normally use the real name, but if USE_ALIAS is true we use the
// alias name itself.

void
Named_type::append_symbol_type_name(Gogo* gogo, bool use_alias,
				    std::string* ret,
				    bool* is_non_identifier) const
{
  if (this->is_error_)
    return;
  if (this->is_alias_ && !use_alias)
    {
      if (this->seen_alias_)
	return;
      this->seen_alias_ = true;
      this->append_mangled_name(this->type_, gogo, ret, is_non_identifier);
      this->seen_alias_ = false;
      return;
    }
  Named_object* no = this->named_object_;
  std::string name;
  if (this->is_builtin())
    go_assert(this->in_function_ == NULL);
  else
    {
      if (this->in_function_ != NULL)
	{
	  const Typed_identifier* rcvr =
	    this->in_function_->func_value()->type()->receiver();
	  if (rcvr != NULL)
	    {
	      Backend_name bname;
	      rcvr->type()->deref()->backend_name(gogo, &bname);
	      ret->append(bname.name());
	      if (bname.is_non_identifier())
		*is_non_identifier = true;
	    }
	  else if (this->in_function_->package() == NULL)
	    ret->append(gogo->pkgpath());
	  else
	    ret->append(this->in_function_->package()->pkgpath());
	  ret->push_back('.');
	  ret->append(Gogo::unpack_hidden_name(this->in_function_->name()));
	}
      else
	{
	  if (no->package() == NULL)
	    ret->append(gogo->pkgpath());
	  else
	    ret->append(no->package()->pkgpath());
	}
      ret->push_back('.');
    }

  ret->append(Gogo::unpack_hidden_name(no->name()));

  if (this->in_function_ != NULL && this->in_function_index_ > 0)
    {
      char buf[30];
      snprintf(buf, sizeof buf, ".i%u", this->in_function_index_);
      ret->append(buf);
    }
}

// Given a name which may or may not have been hidden, append the
// appropriate version of the name to the result string.

void
Gogo::append_possibly_hidden_name(std::string *result, const std::string& name)
{
  if (!Gogo::is_hidden_name(name))
    *result += name;
  else
    *result += name.substr(1);
}

// Set BNAME to the name for the type descriptor symbol for TYPE.
// This can be a global, common, or local symbol, depending.  NT is
// not NULL if it is the name to use.

void
Gogo::type_descriptor_backend_name(const Type* type, Named_type* nt,
				   Backend_name* bname)
{
  // The type descriptor symbol for the unsafe.Pointer type is defined
  // in libgo/runtime/go-unsafe-pointer.c, so just use a reference to
  // that symbol for all unsafe pointer types.
  if (type->is_unsafe_pointer_type())
    {
      bname->set_asm_name("unsafe.Pointer..d");
      return;
    }

  bool is_pointer = false;
  if (nt == NULL && type->points_to() != NULL)
    {
      nt = type->points_to()->unalias()->named_type();
      is_pointer = true;
    }

  if (nt == NULL)
    {
      // Sanity check: we should never generate a type descriptor for
      // an unnamed primitive type.  For those we should always be
      // using a named type, like "int".
      go_assert(!type->is_basic_type());

      type->backend_name(this, bname);
      bname->set_prefix("type..");
    }
  else
    {
      nt->backend_name(this, bname);
      bname->set_suffix(is_pointer ? "..p" : "..d");
    }
}

// Return the name of the type descriptor list symbol of a package.
// This is passed directly to the backend without further encoding.

std::string
Gogo::type_descriptor_list_symbol(const std::string& pkgpath_symbol)
{
  return pkgpath_symbol + "..types";
}

// Return the name of the list of all type descriptor lists.  This is
// only used in the main package.  This is passed directly to the
// backend without further encoding.

std::string
Gogo::typelists_symbol()
{
  return "go..typelists";
}

// Return the assembler name for the GC symbol for a type.  This is
// used to initialize the gcdata field of a type descriptor.  This is
// a local name never referenced outside of this assembly file.  (Note
// that some type descriptors will initialize the gcdata field with a
// name generated by ptrmask_symbol_name rather than this method.)
// This is passed directly to the backend without further encoding.

std::string
Gogo::gc_symbol_name(Type* type)
{
  Backend_name bname;
  this->type_descriptor_backend_name(type, type->named_type(), &bname);
  bname.append_suffix("..g");
  return bname.asm_name();
}

// Return the assembler name for a ptrmask variable.  PTRMASK_SYM_NAME
// is a base32 string encoding the ptrmask (as returned by
// Ptrmask::symname in types.cc).  This name is used to intialize the
// gcdata field of a type descriptor.  These names are globally
// visible.  (Note that some type descriptors will initialize the
// gcdata field with a name generated by gc_symbol_name rather than
// this method.)  This is passed directly to the backend without
// further encoding.

std::string
Gogo::ptrmask_symbol_name(const std::string& ptrmask_sym_name)
{
  return "gcbits.." + ptrmask_sym_name;
}

// Return the assembler name to use for an interface method table used
// for the ordinary type TYPE converted to the interface type ITYPE.
// IS_POINTER is true if this is for the method set for a pointer
// receiver.  This is passed directly to the backend without further
// encoding.

std::string
Gogo::interface_method_table_name(Interface_type* itype, Type* type,
				  bool is_pointer)
{
  Backend_name iname;
  itype->backend_name(this, &iname);
  Backend_name tname;
  type->backend_name(this, &tname);
  return ((is_pointer ? "pimt.." : "imt..")
	  + iname.asm_name()
	  + ".."
	  + tname.asm_name());
}

// If NAME is a special name with a ".." suffix, return the position
// of that suffix.  This is needed because various special names use
// "..SUFFIX", but unpack_hidden_name just looks for '.', and because
// we don't want to encode the suffix.

size_t
Gogo::special_name_pos(const std::string& name)
{
  size_t pos = name.rfind("..");
  if (pos == std::string::npos)
    return pos;
  std::string suffix(name.substr(pos));
  if (suffix == "..hash"
      || suffix == "..eq"
      || suffix == "..stub"
      || suffix == "..d"
      || suffix == "..f"
      || suffix == "..r"
      || suffix == "..import")
    return pos;
  if ((suffix.compare(2, 4, "func") == 0
       || suffix.compare(2, 4, "init") == 0)
      && Gogo::is_digits(suffix.substr(6)))
    return pos;
  if (suffix.compare(2, 5, "thunk") == 0
      && Gogo::is_digits(suffix.substr(7)))
    return pos;
  return std::string::npos;
}

// Return whether the string is non-empty and contains only digits.

bool
Gogo::is_digits(const std::string& s)
{
  if (s.empty())
    return false;
  for (size_t i = 0; i < s.size(); ++i)
    if (s[i] < '0' || s[i] > '9')
      return false;
  return true;
}

// Class Backend_name.

// Get the user visible name.

std::string
Backend_name::name() const
{
  if (this->is_asm_name_)
    return this->components_[0];

  // If there is some character in the name that can't appear in an
  // identifier, use the assembler name as the user name.  This avoids
  // possible problems in the assembler or debugger.  The usual
  // demangling scheme will still work.  We use a prefix of "g." to
  // tell the debugger about this.
  if (this->is_non_identifier_)
    return "g." + this->asm_name();

  std::string ret;
  if (this->prefix_ != NULL)
    ret.append(this->prefix_);
  for (int i = 0; i < this->count_; i++)
    {
      if (i > 0)
	ret.push_back('.');
      ret.append(this->components_[i]);
    }
  if (!this->suffix_.empty())
    ret.append(this->suffix_);
  return ret;
}

// Get the assembler name.

std::string
Backend_name::asm_name() const
{
  if (this->is_asm_name_)
    return this->components_[0];
  std::string ret;
  if (this->prefix_ != NULL)
    ret.append(this->prefix_);
  for (int i = 0; i < this->count_; i++)
    {
      if (i > 0)
	ret.push_back('.');
      ret.append(go_encode_id(this->components_[i]));
    }
  if (!this->suffix_.empty())
    ret.append(this->suffix_);
  return ret;
}

// Get the assembler name, or the empty string if it is the same as
// the user visible name.

std::string
Backend_name::optional_asm_name() const
{
  if (this->is_asm_name_)
    return "";
  if (this->is_non_identifier_)
    return this->asm_name();
  for (int i = 0; i < this->count_; i++)
    if (go_id_needs_encoding(this->components_[i]))
      return this->asm_name();
  return "";
}
