/* JSON trees
   Copyright (C) 2017-2026 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_JSON_H
#define GCC_JSON_H

#include "label-text.h"

/* Implementation of JSON, a lightweight data-interchange format.

   See http://www.json.org/
   and http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf
   and https://tools.ietf.org/html/rfc7159

   Supports parsing text into a DOM-like tree of json::value *, directly
   creating such trees, and dumping json::value * to text.  */

/* TODO: `libcpp/mkdeps.cc` wants JSON writing support for p1689r5 output;
   extract this code and move to libiberty.  */

namespace json
{

/* Forward decls of json::value and its subclasses (using indentation
   to denote inheritance.  */

class value;
  class object;
  class array;
  class float_number;
  class integer_number;
  class string;
  class literal;

/* An enum for discriminating the subclasses of json::value.  */

enum kind
{
  /* class json::object.  */
  JSON_OBJECT,

  /* class json::array.  */
  JSON_ARRAY,

  /* class json::integer_number.  */
  JSON_INTEGER,

  /* class json::float_number.  */
  JSON_FLOAT,

  /* class json::string.  */
  JSON_STRING,

  /* class json::literal uses these three values to identify the
     particular literal.  */
  JSON_TRUE,
  JSON_FALSE,
  JSON_NULL
};

namespace pointer { // json::pointer

/* Implementation of JSON pointer (RFC 6901).  */

/* A token within a JSON pointer, expressing the parent of a particular
   JSON value, and how it is descended from that parent.

   A JSON pointer can be built as a list of these tokens.  */

struct token
{
  enum class kind
  {
    root_value,
    object_member,
    array_index
  };

  token ();
  token (json::object &parent, const char *member);
  token (json::array &parent, size_t index);
  token (const token &other) = delete;
  token (token &&other) = delete;

  ~token ();

  token &
  operator= (const token &other) = delete;

  token &
  operator= (token &&other);

  json::value *m_parent;
  union u
  {
    char *u_member;
    size_t u_index;
  } m_data;
  enum kind m_kind;
};

} // namespace json::pointer

/* Typesafe way to work with properties in JSON objects.  */

template <typename Traits>
struct property
{
  explicit property (const char *key)
  : m_key (label_text::borrow (key))
  {}

  explicit property (const char *key_prefix, const char *key)
  : m_key (label_text::take (concat (key_prefix, key, nullptr)))
  {}

  label_text m_key;
};

using string_property = property<string>;
using integer_property = property<integer_number>;
using bool_property = property<literal>;
using json_property = property<value>;
using array_of_string_property = property<array>;

template <typename EnumType>
struct enum_traits
{
  typedef EnumType enum_t;

  static enum_t get_unknown_value ();
  static bool maybe_get_value_from_string (const char *, enum_t &out);
  static const char *get_string_for_value (enum_t value);
};

template <typename EnumType>
using enum_property = property<enum_traits<EnumType>>;

/* Base class of JSON value.  */

class value
{
 public:
  virtual ~value () {}
  virtual enum kind get_kind () const = 0;
  virtual void print (pretty_printer *pp, bool formatted) const = 0;
  virtual std::unique_ptr<value> clone () const = 0;

  void dump (FILE *, bool formatted) const;
  void DEBUG_FUNCTION dump () const;

  virtual object *dyn_cast_object () { return nullptr; }
  virtual array *dyn_cast_array () { return nullptr; }
  virtual integer_number *dyn_cast_integer_number () { return nullptr; }
  virtual string *dyn_cast_string () { return nullptr; }

  static int compare (const json::value &val_a, const json::value &val_b);

  const pointer::token &get_pointer_token () const { return m_pointer_token; }

  pointer::token m_pointer_token;
};

/* Subclass of value for objects: a collection of key/value pairs
   preserving the ordering in which keys were inserted.

   Preserving the order eliminates non-determinism in the output,
   making it easier for the user to compare repeated invocations.  */

class object : public value
{
 public:
  ~object ();

  typedef hash_map <char *, value *,
    simple_hashmap_traits<nofree_string_hash, value *> > map_t;

  enum kind get_kind () const final override { return JSON_OBJECT; }
  void print (pretty_printer *pp, bool formatted) const final override;
  std::unique_ptr<value> clone () const final override;

  object *dyn_cast_object () final override { return this; }

  bool is_empty () const { return m_map.is_empty (); }

  void set (const char *key, value *v);

  /* Set the property KEY of this object, requiring V
     to be of a specific json::value subclass.

     This can be used to enforce type-checking, making it easier
     to comply with a schema, e.g.
       obj->set<some_subclass> ("property_name", value)
     leading to a compile-time error if VALUE is not of the
     appropriate subclass.  */
  template <typename JsonType>
  void set (const char *key, std::unique_ptr<JsonType> v)
  {
    set (key, v.release ());
  }

  value *get (const char *key) const;
  const map_t &get_map () const { return m_map; }

  void set_string (const char *key, const char *utf8_value);
  void set_integer (const char *key, long v);
  void set_float (const char *key, double v);

  /* Set to literal true/false.  */
  void set_bool (const char *key, bool v);

  /* Typesafe access to properties by name (such as from a schema).  */
  void set_string (const string_property &property, const char *utf8_value);
  void set_integer (const integer_property &property, long value);
  void set_bool (const bool_property &property, bool value);
  void set_array_of_string (const array_of_string_property &property,
			    std::unique_ptr<json::array> value);
  template <typename EnumType>
  bool maybe_get_enum (const enum_property<EnumType> &property,
		       EnumType &out) const;
  template <typename EnumType>
  void set_enum (const enum_property<EnumType> &property,
		 EnumType value);

  static int compare (const json::object &obj_a, const json::object &obj_b);

  size_t get_num_keys () const { return m_keys.length (); }
  const char *get_key (size_t i) const { return m_keys[i]; }

  std::unique_ptr<object> clone_as_object () const;

 private:
  map_t m_map;

  /* Keep track of order in which keys were inserted.  */
  auto_vec <const char *> m_keys;
};

/* Subclass of value for arrays.  */

class array : public value
{
 public:
  ~array ();

  enum kind get_kind () const final override { return JSON_ARRAY; }
  void print (pretty_printer *pp, bool formatted) const final override;
  std::unique_ptr<value> clone () const final override;

  array *dyn_cast_array () final override { return this; }

  void append (value *v);
  void append_string (const char *utf8_value);

  /* Append V to this array, requiring V
     to be a specific json::value subclass.

     This can be used to enforce type-checking, making it easier
     to comply with a schema, e.g.
       arr->append<some_subclass> (value)
     leading to a compile-time error if VALUE is not of the
     appropriate subclass.  */
  template <typename JsonType>
  void append (std::unique_ptr<JsonType> v)
  {
    append (v.release ());
  }

  size_t size () const { return m_elements.length (); }
  value *operator[] (size_t i) const { return m_elements[i]; }

  value **begin () { return m_elements.begin (); }
  value **end () { return m_elements.end (); }
  const value * const *begin () const { return m_elements.begin (); }
  const value * const *end () const { return m_elements.end (); }
  size_t length () const { return m_elements.length (); }
  value *get (size_t idx) const { return m_elements[idx]; }

 private:
  auto_vec<value *> m_elements;
};

/* Subclass of value for floating-point numbers.  */

class float_number : public value
{
 public:
  float_number (double value) : m_value (value) {}

  enum kind get_kind () const final override { return JSON_FLOAT; }
  void print (pretty_printer *pp, bool formatted) const final override;
  std::unique_ptr<value> clone () const final override;

  double get () const { return m_value; }

 private:
  double m_value;
};

/* Subclass of value for integer-valued numbers.  */

class integer_number : public value
{
 public:
  integer_number (long value) : m_value (value) {}

  enum kind get_kind () const final override { return JSON_INTEGER; }
  void print (pretty_printer *pp, bool formatted) const final override;
  std::unique_ptr<value> clone () const final override;

  integer_number *dyn_cast_integer_number () final override { return this; }

  long get () const { return m_value; }

 private:
  long m_value;
};


/* Subclass of value for strings.  */

class string : public value
{
 public:
  explicit string (const char *utf8);
  string (const char *utf8, size_t len);
  ~string () { free (m_utf8); }

  enum kind get_kind () const final override { return JSON_STRING; }
  void print (pretty_printer *pp, bool formatted) const final override;
  std::unique_ptr<value> clone () const final override;
  string *dyn_cast_string () final override { return this; }

  const char *get_string () const { return m_utf8; }
  size_t get_length () const { return m_len; }

 private:
  char *m_utf8;
  size_t m_len;
};

/* Subclass of value for the three JSON literals "true", "false",
   and "null".  */

class literal : public value
{
 public:
  literal (enum kind kind) : m_kind (kind) {}

  /* Construct literal for a boolean value.  */
  literal (bool value): m_kind (value ? JSON_TRUE : JSON_FALSE) {}

  enum kind get_kind () const final override { return m_kind; }
  void print (pretty_printer *pp, bool formatted) const final override;
  std::unique_ptr<value> clone () const final override;

 private:
  enum kind m_kind;
};


template <typename EnumType>
inline bool
object::maybe_get_enum (const enum_property<EnumType> &property,
			EnumType &out) const
{
  if (value *jv = get (property.m_key.get ()))
    if (string *jstr = jv->dyn_cast_string ())
      {
	if (enum_traits<EnumType>::maybe_get_value_from_string
	    (jstr->get_string (), out))
	  return true;
      }
  return false;
}

template <typename EnumType>
inline void
object::set_enum (const enum_property<EnumType> &property,
		  EnumType value)
{
  const char *str
    = json::enum_traits<EnumType>::get_string_for_value (value);
  set_string (property.m_key.get (), str);
}

} // namespace json

template <>
template <>
inline bool
is_a_helper <json::value *>::test (json::value *)
{
  return true;
}

template <>
template <>
inline bool
is_a_helper <const json::value *>::test (const json::value *)
{
  return true;
}

template <>
template <>
inline bool
is_a_helper <json::object *>::test (json::value *jv)
{
  return jv->get_kind () == json::JSON_OBJECT;
}

template <>
template <>
inline bool
is_a_helper <const json::object *>::test (const json::value *jv)
{
  return jv->get_kind () == json::JSON_OBJECT;
}

template <>
template <>
inline bool
is_a_helper <json::array *>::test (json::value *jv)
{
  return jv->get_kind () == json::JSON_ARRAY;
}

template <>
template <>
inline bool
is_a_helper <const json::array *>::test (const json::value *jv)
{
  return jv->get_kind () == json::JSON_ARRAY;
}

template <>
template <>
inline bool
is_a_helper <json::float_number *>::test (json::value *jv)
{
  return jv->get_kind () == json::JSON_FLOAT;
}

template <>
template <>
inline bool
is_a_helper <const json::float_number *>::test (const json::value *jv)
{
  return jv->get_kind () == json::JSON_FLOAT;
}

template <>
template <>
inline bool
is_a_helper <json::integer_number *>::test (json::value *jv)
{
  return jv->get_kind () == json::JSON_INTEGER;
}

template <>
template <>
inline bool
is_a_helper <const json::integer_number *>::test (const json::value *jv)
{
  return jv->get_kind () == json::JSON_INTEGER;
}

template <>
template <>
inline bool
is_a_helper <json::string *>::test (json::value *jv)
{
  return jv->get_kind () == json::JSON_STRING;
}

template <>
template <>
inline bool
is_a_helper <const json::string *>::test (const  json::value *jv)
{
  return jv->get_kind () == json::JSON_STRING;
}

template <>
template <>
inline bool
is_a_helper<json::literal *>::test (json::value *jv)
{
  return (jv->get_kind () == json::JSON_TRUE
	  || jv->get_kind () == json::JSON_FALSE
	  || jv->get_kind () == json::JSON_NULL);
}

template <>
template <>
inline bool
is_a_helper<const json::literal *>::test (const json::value *jv)
{
  return (jv->get_kind () == json::JSON_TRUE
	  || jv->get_kind () == json::JSON_FALSE
	  || jv->get_kind () == json::JSON_NULL);
}

#if CHECKING_P

namespace selftest {

class location;

extern void assert_print_eq (const location &loc,
			     const json::value &jv,
			     bool formatted,
			     const char *expected_json);

} // namespace selftest

#endif /* #if CHECKING_P */

#endif  /* GCC_JSON_H  */
