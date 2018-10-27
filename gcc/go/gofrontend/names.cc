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

// Our external names contain only ASCII alphanumeric characters,
// underscore, and dot.  (According to the GCC sources, dot is not
// permitted in assembler symbols on VxWorks and MMIX.  We will not
// support those systems.)  Go names can not contain dot, so we rely
// on using dot to encode Unicode characters, and to separate Go
// symbols by package, and so forth.  We assume that none of the
// non-Go symbols in the final link will contain a dot, so we don't
// worry about conflicts.
//
// We first describe the basic symbol names, used to represent Go
// functions and variables.  These never start with a dot, never end
// with a dot, never contain two consecutive dots, and never contain a
// dot followed by a digit.
//
// The external name for a normal Go symbol NAME, a function or
// variable, is simply "PKGPATH.NAME".  Note that NAME is not the
// packed form used for the "hidden" name internally in the compiler;
// it is the name that appears in the source code.  PKGPATH is the
// -fgo-pkgpath option as adjusted by Gogo::pkgpath_for_symbol. Note
// that PKGPATH can not contain a dot and neither can NAME.  Also,
// NAME may not begin with a digit.  NAME may require further encoding
// for non-ASCII characters as described below, but until that
// encoding these symbols contain exactly one dot, and they do not
// start with a dot.
//
// The external name for a method NAME for a named type TYPE is
// "PKGPATH.TYPE.NAME".  Unlike the gc compiler, the external name
// does not indicate whether this is a pointer method or a value
// method; a named type can not have both a pointer and value method
// with the same name, so there is no ambiguity.  PKGPATH is the
// package path of the package in which TYPE is defined.  Here none of
// PKGPATH, TYPE, or NAME can be empty or contain a dot, and neither
// TYPE nor NAME may begin with a digit.  Before encoding these names
// contain exactly two dots, not consecutive, and they do not start
// with a dot.
//
// It's uncommon, but the use of type literals with embedded fields
// can cause us to have methods on unnamed types.  The external names
// for these are also PKGPATH.TYPE.NAME, where TYPE is an
// approximately readable version of the type literal, described
// below.  As the type literal encoding always contains multiple dots,
// these names always contain more than two dots.  Although the type
// literal encoding contains dots, neither PKGPATH nor NAME can
// contain a dot, and neither TYPE nor NAME can begin with a digit.
// The effect is that PKGPATH is always the portion of the name before
// the first dot and NAME is always the portion after the last dot.
// There is no ambiguity as long as encoded type literals are
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
// name is ".thunkNN" where NN is a sequence of digits (these
// functions are never globally visible).  Thus the final name of a
// thunk will be PKGPATH..thunkNN.
//
// An init function is treated as a function whose name is ".initNN"
// where NN is a sequence of digits (these functions are never
// globally visible).  Thus the final name of an init function will be
// PKGPATH..initNN.
//
// A nested function is given the name of outermost enclosing function
// or method with an added suffix "..funcNN" where NN is a sequence of
// digits.  Note that the function descriptor of a nested function, if
// needed, will end with "..funcNN..f".
//
// A recover thunk is the same as the name of the function with an
// added suffix "..r".
//
// The name of a type descriptor for a named type is PKGPATH.TYPE..d.
//
// The name of a type descriptor for an unnamed type is type..TYPE.
// That is, the string "type.." followed by the type literal encoding.
// These names are common symbols, in the linker's sense of the word
// common: in the final executable there is only one instance of the
// type descriptor for a given unnamed type.  The type literal
// encoding can never start with a digit or with 'u' or 'U'.
//
// The name of the GC symbol for a named type is PKGPATH.TYPE..g.
//
// The name of the GC symbol for an unnamed type is typeg..TYPE.
// These are common symbols.
//
// The name of a ptrmask symbol is gcbits..B32 where B32 is an
// encoding of the ptrmask bits using only ASCII letters without 'u'
// or 'U'.  These are common symbols.
//
// An interface method table for assigning the non-interface type TYPE
// to the interface type ITYPE is named imt..ITYPE..TYPE.  If ITYPE or
// TYPE is a named type, they are written as PKGPATH.TYPE.  Otherwise
// they are written as a type literal.  An interface method table for
// a pointer method set uses pimt instead of imt.
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
// PKGPATH..import.
//
// The type literal encoding is essentially a single line version of
// the type literal, such as "struct { pkgpath.i int; J int }".  In
// this representation unexported names use their pkgpath, exported
// names omit it.
//
// The type literal encoding is not quite valid Go, as some aspects of
// compiler generated types can not be represented.  For example,
// incomparable struct types have an extra field "{x}".  Struct tags
// are quoted inside curly braces, rather than introduce an encoding
// for quotes.  Struct tags can contain any character, so any single
// byte Unicode character that is not alphanumeric or underscore is
// replaced with .xNN where NN is the hex encoding.
//
// There is a simple encoding for glue characters in type literals:
//   .0 - ' '
//   .1 - '*'
//   .2 - ';'
//   .3 - ','
//   .4 - '{'
//   .5 - '}'
//   .6 - '['
//   .7 - ']'
//   .8 - '('
//   .9 - ')'
// This is unambiguous as, although the type literal can contain a dot
// as shown above, those dots are always followed by a name and names
// can not begin with a digit.  A dot is always followed by a name or
// a digit, and a type literal can neither start nor end with a dot,
// so this never introduces consecutive dots.
//
// Struct tags can contain any character, so they need special
// treatment.  Alphanumerics, underscores, and Unicode characters that
// require more than a single byte are left alone (Unicode characters
// will be encoded later, as described below).  Other single bytes
// characters are replace with .xNN where NN is the hex encoding.
//
// Since Go identifiers can contain Unicode characters, we must encode
// them into ASCII.  We do this last, after the name is generated as
// described above and after type literals are encoded.  To make the
// encoding unambiguous, we introduce it with two consecutive dots.
// This is followed by the letter u and four hex digits or the letter
// U and eight digits, just as in the language only using ..u and ..U
// instead of \u and \U.  The compiler also produces identifiers that
// are qualified by package path, which means that there may also be ASCII
// characters that are not assembler-friendly (ex: '=', '/'). The encoding
// scheme translates such characters into the "..zNN" where NN is the
// hex value for the character. Since before this encoding names can never
// contain consecutive dots followed by 'z', 'u' or 'U', and after this
// encoding "..z", "..u" and "..U" are followed by a known number of
// characters, this is unambiguous.
//
// Demangling these names is straightforward:
//  - replace ..zXX with an ASCII character
//  - replace ..uXXXX with a unicode character
//  - replace ..UXXXXXXXX with a unicode character
//  - replace .D, where D is a digit, with the character from the above
// That will get you as close as possible to a readable name.

// Return the assembler name to use for an exported function, a
// method, or a function/method declaration.  This is not called if
// the function has been given an explicit name via a magic //extern
// or //go:linkname comment.  GO_NAME is the name that appears in the
// Go code.  PACKAGE is the package where the function is defined, and
// is NULL for the package being compiled.  For a method, RTYPE is
// the method's receiver type; for a function, RTYPE is NULL.

std::string
Gogo::function_asm_name(const std::string& go_name, const Package* package,
			const Type* rtype)
{
  std::string ret;
  if (rtype != NULL)
    ret = rtype->deref()->mangled_name(this);
  else if (package == NULL)
    ret = this->pkgpath();
  else
    ret = package->pkgpath();
  ret.push_back('.');
  // Check for special names that will break if we use
  // Gogo::unpack_hidden_name.
  if (Gogo::is_special_name(go_name))
    ret.append(go_name);
  else
    ret.append(Gogo::unpack_hidden_name(go_name));
  return go_encode_id(ret);
}

// Return the name to use for a function descriptor.  These symbols
// are globally visible.

std::string
Gogo::function_descriptor_name(Named_object* no)
{
  if (no->is_function() && !no->func_value()->asm_name().empty())
    return no->func_value()->asm_name() + "..f";
  else if (no->is_function_declaration()
	   && !no->func_declaration_value()->asm_name().empty())
    return no->func_declaration_value()->asm_name() + "..f";
  std::string ret = this->function_asm_name(no->name(), no->package(), NULL);
  ret.append("..f");
  return ret;
}

// Return the name to use for a generated stub method.  MNAME is the
// method name.  PACKAGE is the package where the type that needs this
// stub method is defined.  These functions are globally visible.
// Note that this is the function name that corresponds to the name
// used for the method in Go source code, if this stub method were
// written in Go.  The assembler name will be generated by
// Gogo::function_asm_name, and because this is a method that name
// will include the receiver type.

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
  // imported embedded type.  We need to disambiguate the method name.
  std::string ret = mpkgpath;
  ret.push_back('.');
  ret.append(Gogo::unpack_hidden_name(mname));
  ret.append("..stub");
  return ret;
}

// Return the names of the hash and equality functions for TYPE.  If
// NAME is not NULL it is the name of the type.  Set *HASH_NAME and
// *EQUAL_NAME.

void
Gogo::specific_type_function_names(const Type* type, const Named_type* name,
				   std::string *hash_name,
				   std::string *equal_name)
{
  const Type* rtype = type;
  if (name != NULL)
    rtype = name;
  std::string tname = rtype->mangled_name(this);
  *hash_name = tname + "..hash";
  *equal_name = tname + "..eq";
}

// Return the assembler name to use for a global variable.  GO_NAME is
// the name that appears in the Go code.  PACKAGE is the package where
// the variable is defined, and is NULL for the package being
// compiled.

std::string
Gogo::global_var_asm_name(const std::string& go_name, const Package* package)
{
  std::string ret;
  if (package == NULL)
    ret = this->pkgpath();
  else
    ret = package->pkgpath();
  ret.append(1, '.');
  ret.append(Gogo::unpack_hidden_name(go_name));
  return go_encode_id(ret);
}

// Return an erroneous name that indicates that an error has already
// been reported.

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

// Return a name for a thunk object.

std::string
Gogo::thunk_name()
{
  static int thunk_count;
  char thunk_name[50];
  snprintf(thunk_name, sizeof thunk_name, "..thunk%d", thunk_count);
  ++thunk_count;
  std::string ret = this->pkgpath();
  return ret + thunk_name;
}

// Return whether a function is a thunk.

bool
Gogo::is_thunk(const Named_object* no)
{
  const std::string& name(no->name());
  size_t i = name.find("..thunk");
  if (i == std::string::npos)
    return false;
  for (i += 7; i < name.size(); ++i)
    if (name[i] < '0' || name[i] > '9')
      return false;
  return true;
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
  std::string ret = this->pkgpath();
  return ret + buf;
}

// Return the name to use for a nested function.

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
	  prefix = rcvr->type()->mangled_name(this);
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
// If the function is a method, RTYPE is the receiver type.

std::string
Gogo::recover_thunk_name(const std::string& name, const Type* rtype)
{
  std::string ret;
  if (rtype != NULL)
    {
      ret = rtype->mangled_name(this);
      ret.append(1, '.');
    }
  if (Gogo::is_special_name(name))
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

// Return the name of the variable used to represent the zero value of
// a map.  This is a globally visible common symbol.

std::string
Gogo::map_zero_value_name()
{
  return "go..zerovalue";
}

// Return the name to use for the import control function.

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

// Return a mangled name for a type.  These names appear in symbol
// names in the assembler file for things like type descriptors and
// methods.

std::string
Type::mangled_name(Gogo* gogo) const
{
  std::string ret;

  // The do_mangled_name virtual function will set RET to the mangled
  // name before glue character mapping.
  this->do_mangled_name(gogo, &ret);

  // Type descriptor names and interface method table names use a ".."
  // before the mangled name of a type, so to avoid ambiguity the
  // mangled name must not start with 'u' or 'U' or a digit.
  go_assert((ret[0] < '0' || ret[0] > '9') && ret[0] != ' ');
  if (ret[0] == 'u' || ret[0] == 'U')
    ret = " " + ret;

  // Map glue characters as described above.

  // The mapping is only unambiguous if there is no .DIGIT in the
  // string, so check that.
  for (size_t i = ret.find('.');
       i != std::string::npos;
       i = ret.find('.', i + 1))
    {
      if (i + 1 < ret.size())
	{
	  char c = ret[i + 1];
	  go_assert(c < '0' || c > '9');
	}
    }

  // The order of these characters is the replacement code.
  const char * const replace = " *;,{}[]()";

  const size_t rlen = strlen(replace);
  char buf[2];
  buf[0] = '.';
  for (size_t ri = 0; ri < rlen; ++ri)
    {
      buf[1] = '0' + ri;
      while (true)
	{
	  size_t i = ret.find(replace[ri]);
	  if (i == std::string::npos)
	    break;
	  ret.replace(i, 1, buf, 2);
	}
    }

  return ret;
}

// The mangled name is implemented as a method on each instance of
// Type.

void
Error_type::do_mangled_name(Gogo*, std::string* ret) const
{
  ret->append("{error}");
}

void
Void_type::do_mangled_name(Gogo*, std::string* ret) const
{
  ret->append("{void}");
}

void
Boolean_type::do_mangled_name(Gogo*, std::string* ret) const
{
  ret->append("bool");
}

void
Integer_type::do_mangled_name(Gogo*, std::string* ret) const
{
  char buf[100];
  snprintf(buf, sizeof buf, "%s%si%d",
	   this->is_abstract_ ? "{abstract}" : "",
	   this->is_unsigned_ ? "u" : "",
	   this->bits_);
  ret->append(buf);
}

void
Float_type::do_mangled_name(Gogo*, std::string* ret) const
{
  char buf[100];
  snprintf(buf, sizeof buf, "%sfloat%d",
	   this->is_abstract_ ? "{abstract}" : "",
	   this->bits_);
  ret->append(buf);
}

void
Complex_type::do_mangled_name(Gogo*, std::string* ret) const
{
  char buf[100];
  snprintf(buf, sizeof buf, "%sc%d",
	   this->is_abstract_ ? "{abstract}" : "",
	   this->bits_);
  ret->append(buf);
}

void
String_type::do_mangled_name(Gogo*, std::string* ret) const
{
  ret->append("string");
}

void
Function_type::do_mangled_name(Gogo* gogo, std::string* ret) const
{
  ret->append("func");

  if (this->receiver_ != NULL)
    {
      ret->push_back('(');
      this->append_mangled_name(this->receiver_->type(), gogo, ret);
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
	    {
	      // We can't use "..." here because the mangled name
	      // might start with 'u' or 'U', which would be ambiguous
	      // with the encoding of Unicode characters.
	      ret->append(",,,");
	    }
	  this->append_mangled_name(p->type(), gogo, ret);
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
	  this->append_mangled_name(p->type(), gogo, ret);
	}
    }
  ret->push_back(')');
}

void
Pointer_type::do_mangled_name(Gogo* gogo, std::string* ret) const
{
  ret->push_back('*');
  this->append_mangled_name(this->to_type_, gogo, ret);
}

void
Nil_type::do_mangled_name(Gogo*, std::string* ret) const
{
  ret->append("{nil}");
}

void
Struct_type::do_mangled_name(Gogo* gogo, std::string* ret) const
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

	  // For an anonymous field with an alias type, the field name
	  // is the alias name.
	  if (p->is_anonymous()
	      && p->type()->named_type() != NULL
	      && p->type()->named_type()->is_alias())
	    p->type()->named_type()->append_mangled_type_name(gogo, true, ret);
	  else
	    this->append_mangled_name(p->type(), gogo, ret);

	  if (p->has_tag())
	    {
	      // Use curly braces around a struct tag, since they are
	      // unambiguous here and we have no encoding for
	      // quotation marks.
	      ret->push_back('{');
	      ret->append(go_mangle_struct_tag(p->tag()));
	      ret->push_back('}');
	    }
	}
    }

  ret->push_back('}');
}

void
Array_type::do_mangled_name(Gogo* gogo, std::string* ret) const
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
  this->append_mangled_name(this->element_type_, gogo, ret);
}

void
Map_type::do_mangled_name(Gogo* gogo, std::string* ret) const
{
  ret->append("map[");
  this->append_mangled_name(this->key_type_, gogo, ret);
  ret->push_back(']');
  this->append_mangled_name(this->val_type_, gogo, ret);
}

void
Channel_type::do_mangled_name(Gogo* gogo, std::string* ret) const
{
  if (!this->may_send_)
    ret->append("{}");
  ret->append("chan");
  if (!this->may_receive_)
    ret->append("{}");
  ret->push_back(' ');
  this->append_mangled_name(this->element_type_, gogo, ret);
}

void
Interface_type::do_mangled_name(Gogo* gogo, std::string* ret) const
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

	  this->append_mangled_name(p->type(), gogo, ret);
	}
      this->seen_ = false;
    }

  ret->push_back('}');
}

void
Named_type::do_mangled_name(Gogo* gogo, std::string* ret) const
{
  this->append_mangled_type_name(gogo, false, ret);
}

void
Forward_declaration_type::do_mangled_name(Gogo* gogo, std::string* ret) const
{
  if (this->is_defined())
    this->append_mangled_name(this->real_type(), gogo, ret);
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

// Append the mangled name for a named type to RET.  For an alias we
// normally use the real name, but if USE_ALIAS is true we use the
// alias name itself.

void
Named_type::append_mangled_type_name(Gogo* gogo, bool use_alias,
				     std::string* ret) const
{
  if (this->is_error_)
    return;
  if (this->is_alias_ && !use_alias)
    {
      if (this->seen_alias_)
	return;
      this->seen_alias_ = true;
      this->append_mangled_name(this->type_, gogo, ret);
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
	    ret->append(rcvr->type()->deref()->mangled_name(gogo));
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
      snprintf(buf, sizeof buf, "..i%u", this->in_function_index_);
      ret->append(buf);
    }
}

// Return the name for the type descriptor symbol for TYPE.  This can
// be a global, common, or local symbol, depending.  NT is not NULL if
// it is the name to use.

std::string
Gogo::type_descriptor_name(Type* type, Named_type* nt)
{
  // The type descriptor symbol for the unsafe.Pointer type is defined
  // in libgo/runtime/go-unsafe-pointer.c, so just use a reference to
  // that symbol for all unsafe pointer types.
  if (type->is_unsafe_pointer_type())
    return "unsafe.Pointer..d";

  if (nt == NULL)
    return "type.." + type->mangled_name(this);

  std::string ret;
  Named_object* no = nt->named_object();
  unsigned int index;
  const Named_object* in_function = nt->in_function(&index);
  if (nt->is_builtin())
    go_assert(in_function == NULL);
  else
    {
      if (in_function != NULL)
	{
	  const Typed_identifier* rcvr =
	    in_function->func_value()->type()->receiver();
	  if (rcvr != NULL)
	    ret.append(rcvr->type()->deref()->mangled_name(this));
	  else if (in_function->package() == NULL)
	    ret.append(this->pkgpath());
	  else
	    ret.append(in_function->package()->pkgpath());
	  ret.push_back('.');
	  ret.append(Gogo::unpack_hidden_name(in_function->name()));
	  ret.push_back('.');
	}

      if (no->package() == NULL)
	ret.append(this->pkgpath());
      else
	ret.append(no->package()->pkgpath());
      ret.push_back('.');
    }

  Gogo::append_possibly_hidden_name(&ret, no->name());

  if (in_function != NULL && index > 0)
    {
      char buf[30];
      snprintf(buf, sizeof buf, "..i%u", index);
      ret.append(buf);
    }

  ret.append("..d");

  return ret;
}

// Return the name for the GC symbol for a type.  This is used to
// initialize the gcdata field of a type descriptor.  This is a local
// name never referenced outside of this assembly file.  (Note that
// some type descriptors will initialize the gcdata field with a name
// generated by ptrmask_symbol_name rather than this method.)

std::string
Gogo::gc_symbol_name(Type* type)
{
  return this->type_descriptor_name(type, type->named_type()) + "..g";
}

// Return the name for a ptrmask variable.  PTRMASK_SYM_NAME is a
// base32 string encoding the ptrmask (as returned by Ptrmask::symname
// in types.cc).  This name is used to intialize the gcdata field of a
// type descriptor.  These names are globally visible.  (Note that
// some type descriptors will initialize the gcdata field with a name
// generated by gc_symbol_name rather than this method.)

std::string
Gogo::ptrmask_symbol_name(const std::string& ptrmask_sym_name)
{
  return "gcbits.." + ptrmask_sym_name;
}

// Return the name to use for an interface method table used for the
// ordinary type TYPE converted to the interface type ITYPE.
// IS_POINTER is true if this is for the method set for a pointer
// receiver.

std::string
Gogo::interface_method_table_name(Interface_type* itype, Type* type,
				  bool is_pointer)
{
  return ((is_pointer ? "pimt.." : "imt..")
	  + itype->mangled_name(this)
	  + ".."
	  + type->mangled_name(this));
}

// Return whether NAME is a special name that can not be passed to
// unpack_hidden_name.  This is needed because various special names
// use "..SUFFIX", but unpack_hidden_name just looks for '.'.

bool
Gogo::is_special_name(const std::string& name)
{
  return (name.find("..hash") != std::string::npos
	  || name.find("..eq") != std::string::npos
	  || name.find("..stub") != std::string::npos
	  || name.find("..func") != std::string::npos
	  || name.find("..r") != std::string::npos
	  || name.find("..init") != std::string::npos
	  || name.find("..thunk") != std::string::npos
	  || name.find("..import") != std::string::npos);
}
