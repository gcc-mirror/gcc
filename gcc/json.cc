/* JSON trees
   Copyright (C) 2017-2025 Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "json.h"
#include "pretty-print.h"
#include "math.h"
#include "selftest.h"

using namespace json;

/* Print a JSON string to PP, escaping '"', control characters,
   and embedded null bytes.
   The string is required to be UTF-8 encoded.  */

static void
print_escaped_json_string (pretty_printer *pp,
			   const char *utf8_str,
			   size_t len)
{
  pp_character (pp, '"');
  for (size_t i = 0; i != len; ++i)
    {
      char ch = utf8_str[i];
      switch (ch)
	{
	case '"':
	  pp_string (pp, "\\\"");
	  break;
	case '\\':
	  pp_string (pp, "\\\\");
	  break;
	case '\b':
	  pp_string (pp, "\\b");
	  break;
	case '\f':
	  pp_string (pp, "\\f");
	  break;
	case '\n':
	  pp_string (pp, "\\n");
	  break;
	case '\r':
	  pp_string (pp, "\\r");
	  break;
	case '\t':
	  pp_string (pp, "\\t");
	  break;
	case '\0':
	  pp_string (pp, "\\0");
	  break;
	default:
	  pp_character (pp, ch);
	}
    }
  pp_character (pp, '"');
}

/* class pointer::token.  */

pointer::token::token ()
{
  m_parent = nullptr;
  m_data.u_member = nullptr;
  m_kind = kind::root_value;
}

pointer::token::token (json::object &parent, const char *member)
{
  m_parent = &parent;
  m_data.u_member = xstrdup (member); // ideally we'd share
  m_kind = kind::object_member;
}

pointer::token::token (json::array &parent, size_t index)
{
  m_parent = &parent;
  m_data.u_index = index;
  m_kind = kind::array_index;
}

pointer::token::~token ()
{
  if (m_kind == kind::object_member)
    {
      gcc_assert (m_data.u_member);
      free (m_data.u_member);
    }
}

pointer::token &
pointer::token::operator= (pointer::token &&other)
{
  m_parent = other.m_parent;
  m_data = other.m_data;
  m_kind = other.m_kind;

  other.m_parent = nullptr;
  other.m_data.u_member = nullptr;
  other.m_kind = kind::root_value;

  return *this;
}

/* class json::value.  */

/* Dump this json::value tree to OUTF.

   The key/value pairs of json::objects are printed in the order
   in which the keys were originally inserted.  */

void
value::dump (FILE *outf, bool formatted) const
{
  pretty_printer pp;
  pp_buffer (&pp)->m_stream = outf;
  print (&pp, formatted);
  pp_flush (&pp);
}

/* A convenience function for debugging.
   Dump to stderr with formatting, and a trailing newline. */

void
value::dump () const
{
  dump (stderr, true);
  fprintf (stderr, "\n");
}

/* A deterministic total ordering for comparing json values, so that we
   can e.g. put them in std::map.

   This is intended to follow the condition for equality described in
   the JSON Schema standard (§4.3, “Instance equality”), as referenced
   by SARIF v2.1.0 (§3.7.3 "Array properties with unique values"), but has
   the following limitations:
   - numbers are supposed to be checked for "the same mathematical value",
   but in this implementation int vs float numbers won't compare as equal,
   and float number comparison is bitwise
   - strings are supposed to be "the same codepoint-for-codepoint", but
   this implementation doesn't take into account canonicalization issues.  */

int
value::compare (const value &val_a, const value &val_b)
{
  enum kind kind_a = val_a.get_kind ();
  enum kind kind_b = val_b.get_kind ();
  if (kind_a != kind_b)
    return (int)kind_a - (int)kind_b;

  switch (kind_a)
    {
    default:
      gcc_unreachable ();

    case JSON_OBJECT:
      {
	const object &obj_a = (const object &)val_a;
	const object &obj_b = (const object &)val_b;
	return object::compare (obj_a, obj_b);
      }
      break;

    case JSON_ARRAY:
      {
	const array &arr_a = (const array &)val_a;
	const array &arr_b = (const array &)val_b;
	if (int cmp_size = (int)arr_a.size () - (int)arr_b.size ())
	  return cmp_size;
	for (size_t idx = 0; idx < arr_a.size (); ++idx)
	  if (int cmp_element = compare (*arr_a[idx], *arr_b[idx]))
	    return cmp_element;
	return 0;
      }
      break;

    case JSON_INTEGER:
      {
	const integer_number &int_a = (const integer_number &)val_a;
	const integer_number &int_b = (const integer_number &)val_b;
	return int_a.get () - int_b.get ();
      }
      break;

    case JSON_FLOAT:
      {
	const float_number &float_a = (const float_number &)val_a;
	const float_number &float_b = (const float_number &)val_b;
	union u
	{
	  double u_double;
	  char u_buf[sizeof(double)];
	};
	union u u_a, u_b;
	u_a.u_double = float_a.get ();
	u_b.u_double = float_b.get ();
	return memcmp (&u_a, &u_b, sizeof(double));
      }
      break;

    case JSON_STRING:
      {
	const string &str_a = (const string &)val_a;
	const string &str_b = (const string &)val_b;
	return strcmp (str_a.get_string (), str_b.get_string ());
      }
      break;

    case JSON_TRUE:
    case JSON_FALSE:
    case JSON_NULL:
      /* All instances of literals compare equal to instances
	 of the same literal.  */
      return 0;
    }
}

/* class json::object, a subclass of json::value, representing
   an ordered collection of key/value pairs.  */

/* json:object's dtor.  */

object::~object ()
{
  for (map_t::iterator it = m_map.begin (); it != m_map.end (); ++it)
    {
      free (const_cast <char *>((*it).first));
      delete ((*it).second);
    }
}

/* Implementation of json::value::print for json::object.  */

void
object::print (pretty_printer *pp, bool formatted) const
{
  pp_character (pp, '{');
  if (formatted)
    pp_indentation (pp) += 1;

  /* Iterate in the order that the keys were inserted.  */
  unsigned i;
  const char *key;
  FOR_EACH_VEC_ELT (m_keys, i, key)
    {
      if (i > 0)
	{
	  pp_string (pp, ",");
	  if (formatted)
	    {
	      pp_newline (pp);
	      pp_indent (pp);
	    }
	  else
	    pp_space (pp);
	}
      map_t &mut_map = const_cast<map_t &> (m_map);
      value *value = *mut_map.get (key);
      print_escaped_json_string (pp, key, strlen (key));
      pp_string (pp, ": ");
      const int indent = strlen (key) + 4;
      if (formatted)
	pp_indentation (pp) += indent;
      value->print (pp, formatted);
      if (formatted)
	pp_indentation (pp) -= indent;
    }
  if (formatted)
    pp_indentation (pp) -= 1;
  pp_character (pp, '}');
}

/* Set the json::value * for KEY, taking ownership of V
   (and taking a copy of KEY if necessary).  */

void
object::set (const char *key, value *v)
{
  gcc_assert (key);
  gcc_assert (v);

  value **ptr = m_map.get (key);
  if (ptr)
    {
      /* If the key is already present, delete the existing value
	 and overwrite it.  */
      delete *ptr;
      *ptr = v;
    }
  else
    {
      /* If the key wasn't already present, take a copy of the key,
	 and store the value.  */
      char *owned_key = xstrdup (key);
      m_map.put (owned_key, v);
      m_keys.safe_push (owned_key);
    }

  v->m_pointer_token = pointer::token (*this, key);
}

/* Get the json::value * for KEY.

   The object retains ownership of the value.  */

value *
object::get (const char *key) const
{
  gcc_assert (key);

  value **ptr = const_cast <map_t &> (m_map).get (key);
  if (ptr)
    return *ptr;
  else
    return NULL;
}

/* Set value of KEY within this object to a JSON
   string value based on UTF8_VALUE.  */

void
object::set_string (const char *key, const char *utf8_value)
{
  set (key, new json::string (utf8_value));
}

/* Set value of KEY within this object to a JSON
   integer value based on V.  */

void
object::set_integer (const char *key, long v)
{
  set (key, new json::integer_number (v));
}

/* Set value of KEY within this object to a JSON
   floating point value based on V.  */

void
object::set_float (const char *key, double v)
{
  set (key, new json::float_number (v));
}

/* Set value of KEY within this object to the JSON
   literal true or false, based on V.  */

void
object::set_bool (const char *key, bool v)
{
  set (key, new json::literal (v));
}

/* Subroutine of json::compare for comparing a pairs of objects.  */

int
object::compare (const json::object &obj_a, const json::object &obj_b)
{
  if (int cmp_size = (int)obj_a.m_keys.length () - (int)obj_b.m_keys.length ())
    return cmp_size;

  for (auto iter_a : obj_a.m_map)
    {
      const char *key = iter_a.first;
      const value *value_a = iter_a.second;
      gcc_assert (value_a);

      const value *value_b = obj_b.get (key);
      if (!value_b)
	/* Key is in OBJ_A but not in OBJ_B.  */
	return 1;
      /* If key is OBJ_B but not in OBJ_A, then the
	 count of keys will have been different, or
	 OBJ_A would have had a key not in OBJ_B.  */
      if (int cmp_value = value::compare (*value_a, *value_b))
	/* Values for key are non-equal.  */
	return cmp_value;
    }

  /* Objects are equal.  */
  return 0;
}

/* class json::array, a subclass of json::value, representing
   an ordered collection of values.  */

/* json::array's dtor.  */

array::~array ()
{
  unsigned i;
  value *v;
  FOR_EACH_VEC_ELT (m_elements, i, v)
    delete v;
}

/* Implementation of json::value::print for json::array.  */

void
array::print (pretty_printer *pp, bool formatted) const
{
  pp_character (pp, '[');
  if (formatted)
    pp_indentation (pp) += 1;
  unsigned i;
  value *v;
  FOR_EACH_VEC_ELT (m_elements, i, v)
    {
      if (i)
	{
	  pp_string (pp, ",");
	  if (formatted)
	    {
	      pp_newline (pp);
	      pp_indent (pp);
	    }
	  else
	    pp_space (pp);
	}
      v->print (pp, formatted);
    }
  if (formatted)
    pp_indentation (pp) -= 1;
  pp_character (pp, ']');
}

/* Append non-NULL value V to a json::array, taking ownership of V.  */

void
array::append (value *v)
{
  gcc_assert (v);
  v->m_pointer_token = pointer::token (*this, m_elements.length ());
  m_elements.safe_push (v);
}

void
array::append_string (const char *utf8_value)
{
  gcc_assert (utf8_value);
  append (new json::string (utf8_value));
}

/* class json::float_number, a subclass of json::value, wrapping a double.  */

/* Implementation of json::value::print for json::float_number.  */

void
float_number::print (pretty_printer *pp,
		     bool formatted ATTRIBUTE_UNUSED) const
{
  char tmp[1024];
  snprintf (tmp, sizeof (tmp), "%g", m_value);
  pp_string (pp, tmp);
}

/* class json::integer_number, a subclass of json::value, wrapping a long.  */

/* Implementation of json::value::print for json::integer_number.  */

void
integer_number::print (pretty_printer *pp,
		       bool formatted ATTRIBUTE_UNUSED) const
{
  char tmp[1024];
  snprintf (tmp, sizeof (tmp), "%ld", m_value);
  pp_string (pp, tmp);
}


/* class json::string, a subclass of json::value.  */

/* json::string's ctor.  */

string::string (const char *utf8)
{
  gcc_assert (utf8);
  m_utf8 = xstrdup (utf8);
  m_len = strlen (utf8);
}

string::string (const char *utf8, size_t len)
{
  gcc_assert (utf8);
  m_utf8 = XNEWVEC (char, len);
  m_len = len;
  memcpy (m_utf8, utf8, len);
}

/* Implementation of json::value::print for json::string.  */

void
string::print (pretty_printer *pp,
	       bool formatted ATTRIBUTE_UNUSED) const
{
  print_escaped_json_string (pp, m_utf8, m_len);
}

/* class json::literal, a subclass of json::value.  */

/* Implementation of json::value::print for json::literal.  */

void
literal::print (pretty_printer *pp,
		bool formatted ATTRIBUTE_UNUSED) const
{
  switch (m_kind)
    {
    case JSON_TRUE:
      pp_string (pp, "true");
      break;
    case JSON_FALSE:
      pp_string (pp, "false");
      break;
    case JSON_NULL:
      pp_string (pp, "null");
      break;
    default:
      gcc_unreachable ();
    }
}


#if CHECKING_P

namespace selftest {

/* Selftests.  */

/* Verify that JV->print () prints EXPECTED_JSON.  */

void
assert_print_eq (const location &loc,
		 const json::value &jv,
		 bool formatted,
		 const char *expected_json)
{
  pretty_printer pp;
  jv.print (&pp, formatted);
  ASSERT_STREQ_AT (loc, expected_json, pp_formatted_text (&pp));
}

#define ASSERT_PRINT_EQ(JV, FORMATTED, EXPECTED_JSON)	\
  assert_print_eq (SELFTEST_LOCATION, JV, FORMATTED, EXPECTED_JSON)

/* Verify that object::get works as expected.  */

static void
test_object_get ()
{
  object obj;
  value *val = new json::string ("value");
  obj.set ("foo", val);
  ASSERT_EQ (obj.get ("foo"), val);
  ASSERT_EQ (obj.get ("not-present"), NULL);
}

/* Verify that JSON objects are written correctly.  */

static void
test_writing_objects ()
{
  object obj;
  obj.set_string ("foo", "bar");
  obj.set_string ("baz", "quux");
  obj.set_string ("\"\\\b\f\n\r\t", "value for awkward key");

  /* This test relies on json::object writing out key/value pairs
     in key-insertion order.  */
  ASSERT_PRINT_EQ (obj, true,
		   "{\"foo\": \"bar\",\n"
		   " \"baz\": \"quux\",\n"
		   " \"\\\"\\\\\\b\\f\\n\\r\\t\": \"value for awkward key\"}");
  ASSERT_PRINT_EQ (obj, false,
		   "{\"foo\": \"bar\", \"baz\": \"quux\""
		   ", \"\\\"\\\\\\b\\f\\n\\r\\t\": \"value for awkward key\"}");
}

/* Verify that JSON arrays are written correctly.  */

static void
test_writing_arrays ()
{
  array arr;
  ASSERT_PRINT_EQ (arr, true, "[]");

  arr.append (new json::string ("foo"));
  ASSERT_PRINT_EQ (arr, true, "[\"foo\"]");

  arr.append_string ("bar");
  ASSERT_PRINT_EQ (arr, true,
		   "[\"foo\",\n"
		   " \"bar\"]");
  ASSERT_PRINT_EQ (arr, false,
		   "[\"foo\", \"bar\"]");
}

/* Verify that JSON numbers are written correctly.  */

static void
test_writing_float_numbers ()
{
  ASSERT_PRINT_EQ (float_number (0), true, "0");
  ASSERT_PRINT_EQ (float_number (42), true, "42");
  ASSERT_PRINT_EQ (float_number (-100), true, "-100");
  ASSERT_PRINT_EQ (float_number (123456789), true, "1.23457e+08");
}

static void
test_writing_integer_numbers ()
{
  ASSERT_PRINT_EQ (integer_number (0), true, "0");
  ASSERT_PRINT_EQ (integer_number (42), true, "42");
  ASSERT_PRINT_EQ (integer_number (-100), true, "-100");
  ASSERT_PRINT_EQ (integer_number (123456789), true, "123456789");
  ASSERT_PRINT_EQ (integer_number (-123456789), true, "-123456789");
}

/* Verify that JSON strings are written correctly.  */

static void
test_writing_strings ()
{
  string foo ("foo");
  ASSERT_PRINT_EQ (foo, true, "\"foo\"");

  string contains_quotes ("before \"quoted\" after");
  ASSERT_PRINT_EQ (contains_quotes, true, "\"before \\\"quoted\\\" after\"");

  const char data[] = {'a', 'b', 'c', 'd', '\0', 'e', 'f'};
  string not_terminated (data, 3);
  ASSERT_PRINT_EQ (not_terminated, true, "\"abc\"");
  string embedded_null (data, sizeof data);
  ASSERT_PRINT_EQ (embedded_null, true, "\"abcd\\0ef\"");
}

/* Verify that JSON literals are written correctly.  */

static void
test_writing_literals ()
{
  ASSERT_PRINT_EQ (literal (JSON_TRUE), true, "true");
  ASSERT_PRINT_EQ (literal (JSON_FALSE), true, "false");
  ASSERT_PRINT_EQ (literal (JSON_NULL), true, "null");

  ASSERT_PRINT_EQ (literal (true), true, "true");
  ASSERT_PRINT_EQ (literal (false), true, "false");
}

/* Verify that nested values are formatted correctly when written.

   Also, make use of array::append(std::unique_ptr<value>) and
   object::set (const char *key, std::unique_ptr<value> v).*/

static void
test_formatting ()
{
  object obj;
  object *child = new object;
  std::unique_ptr<object> grandchild = std::make_unique<object> ();

  obj.set_string ("str", "bar");
  obj.set ("child", child);
  obj.set_integer ("int", 42);

  array *arr = new array;
  for (int i = 0; i < 3; i++)
    arr->append (std::make_unique<integer_number> (i));
  grandchild->set ("arr", arr);
  grandchild->set_integer ("int", 1066);

  child->set ("grandchild", std::move (grandchild));
  child->set_integer ("int", 1776);

  /* This test relies on json::object writing out key/value pairs
     in key-insertion order.  */
  ASSERT_PRINT_EQ (obj, true,
		   ("{\"str\": \"bar\",\n"
		    " \"child\": {\"grandchild\": {\"arr\": [0,\n"
		    "                                  1,\n"
		    "                                  2],\n"
		    "                          \"int\": 1066},\n"
		    "           \"int\": 1776},\n"
		    " \"int\": 42}"));
  ASSERT_PRINT_EQ (obj, false,
		   ("{\"str\": \"bar\", \"child\": {\"grandchild\":"
		    " {\"arr\": [0, 1, 2], \"int\": 1066},"
		    " \"int\": 1776}, \"int\": 42}"));
}

/* Helper function for reporting failure of JSON comparisons.  */

static void
fail_comparison (const location &loc,
		 const char *desc,
		 const value &val_a, const value &val_b,
		 const char *desc_expected_value,
		 int actual_value)
{
  fprintf (stderr, "val_a: ");
  val_a.dump ();

  fprintf (stderr, "val_b: ");
  val_b.dump ();

  selftest::fail_formatted (loc,
			    "%s: failed JSON comparison:"
			    " expected: %s got: %i\n",
			    desc,
			    desc_expected_value, actual_value);
}

/* Implementation of ASSERT_JSON_EQ.  */

static void
assert_json_equal (const location &loc,
		   const char *desc,
		   const value &val_a, const value &val_b)
{
  /* Comparison should return zero, both ways, indicating no differences.  */
  const int a_vs_b = value::compare (val_a, val_b);
  if (a_vs_b != 0)
    fail_comparison (loc, desc, val_a, val_b, "zero", a_vs_b);

  const int b_vs_a = value::compare (val_b, val_a);
  if (b_vs_a != 0)
    fail_comparison (loc, desc, val_b, val_a, "zero", b_vs_a);
}

/* Verify that json::value::compare returns 0 ("no differences") on
   VAL1 and VAL2, in both orders.  */

#define ASSERT_JSON_EQ(VAL1, VAL2) \
  SELFTEST_BEGIN_STMT						\
    assert_json_equal ((SELFTEST_LOCATION),			\
		       "ASSERT_JSON_EQ",			\
		       (VAL1), (VAL2));			\
  SELFTEST_END_STMT

/* Implementation of ASSERT_JSON_NE.  */

static void
assert_json_non_equal (const location &loc,
		       const char *desc,
		       const value &val_a, const value &val_b)
{
  /* Comparison should be non-zero, indicating differences.  */
  const int a_vs_b = value::compare (val_a, val_b);
  if (a_vs_b == 0)
    fail_comparison (loc, desc, val_a, val_b, "non-zero", a_vs_b);

  const int b_vs_a = value::compare (val_b, val_a);
  ASSERT_NE_AT (loc, b_vs_a, 0);
  if (b_vs_a == 0)
    fail_comparison (loc, desc, val_b, val_a, "non-zero", b_vs_a);

  /* Swapping the args should swap the sign of the result
     (but isn't necessarily the negation).  */
  if ( (a_vs_b > 0) == (b_vs_a > 0) )
    fail_comparison (loc, desc, val_b, val_a, "opposite signs", 1);
}

/* Verify that json::value::compare returns non-zero ("different") on
   VAL1 and VAL2, in both orders, and that they have opposite
   sign.  */

#define ASSERT_JSON_NE(VAL1, VAL2) \
  SELFTEST_BEGIN_STMT						\
    assert_json_non_equal ((SELFTEST_LOCATION),		\
			   "ASSERT_JSON_NE",			\
			   (VAL1), (VAL2));			\
  SELFTEST_END_STMT

/* Verify that json::value::compare works as expected.  */

static void
test_comparisons ()
{
  /* Literals.  */

  literal null_lit (JSON_NULL);
  ASSERT_JSON_EQ (null_lit, null_lit);

  literal other_null_lit (JSON_NULL);
  ASSERT_JSON_EQ (null_lit, other_null_lit);

  literal true_lit (JSON_TRUE);
  ASSERT_JSON_EQ (true_lit, true_lit);
  ASSERT_JSON_NE (true_lit, null_lit);

  literal false_lit (JSON_FALSE);
  ASSERT_JSON_EQ (false_lit, false_lit);
  ASSERT_JSON_NE (false_lit, true_lit);
  ASSERT_JSON_NE (false_lit, null_lit);

  /* Strings.  */
  string str_foo_1 ("foo");
  ASSERT_JSON_EQ (str_foo_1, str_foo_1);

  string str_foo_2 ("foo");
  ASSERT_JSON_EQ (str_foo_1, str_foo_2);

  string str_bar ("bar");
  ASSERT_JSON_NE (str_bar, str_foo_1);

  /* Numbers.  */
  integer_number i_42 (42);
  ASSERT_JSON_EQ (i_42, i_42);
  integer_number i_42_2 (42);
  ASSERT_JSON_EQ (i_42, i_42_2);
  integer_number i_43 (43);
  ASSERT_JSON_NE (i_42, i_43);

  float_number f_zero (0.0);
  ASSERT_JSON_EQ (f_zero, f_zero);
  float_number f_zero_2 (0.0);
  ASSERT_JSON_EQ (f_zero, f_zero_2);
  float_number f_one (1.0);
  ASSERT_JSON_NE (f_zero, f_one);
  /* We don't yet test the more awkward cases e.g. NaN.  */

  /* Objects.  */

  // Empty object
  // Self comparison should be 0
  object empty_obj_a;
  ASSERT_JSON_EQ (empty_obj_a, empty_obj_a);

  // Instances of empty objects should compare equal to each other
  object empty_obj_b;
  ASSERT_JSON_EQ (empty_obj_a, empty_obj_b);

  // Object with one field:
  object obj_1;
  obj_1.set_string ("foo", "bar");
  // Self comparison should be 0
  ASSERT_JSON_EQ (obj_1, obj_1);

  // but should be different to an empty object:
  ASSERT_JSON_NE (obj_1, empty_obj_a);

  // Another with one field, with same key/value:
  object obj_2;
  obj_2.set_string ("foo", "bar");
  ASSERT_JSON_EQ (obj_1, obj_2);

  // Same key, different value:
  object obj_3;
  obj_3.set_string ("foo", "baz");
  ASSERT_JSON_NE (obj_1, obj_3);

  // Adding an extra property:
  obj_2.set_integer ("year", 1066);
  ASSERT_JSON_NE (obj_1, obj_2);

  /* Different insertion order, but same k-v pairs should be equal,
     despite having different serialization.  */
  object obj_4;
  obj_4.set_integer ("year", 1066);
  obj_4.set_string ("foo", "bar");
  ASSERT_JSON_EQ (obj_2, obj_4);
  ASSERT_PRINT_EQ (obj_2, false, "{\"foo\": \"bar\", \"year\": 1066}");
  ASSERT_PRINT_EQ (obj_4, false, "{\"year\": 1066, \"foo\": \"bar\"}");

  /* Arrays.  */

  // Empty array
  array empty_arr_a;
  // Self comparison should be 0
  ASSERT_JSON_EQ (empty_arr_a, empty_arr_a);

  // Objects and arrays are different
  ASSERT_JSON_NE (empty_obj_a, empty_arr_a);

  // Instances of empty arrays should compare equal to each other
  array empty_arr_b;
  ASSERT_JSON_EQ (empty_arr_a, empty_arr_b);

  // Array with one element:
  array arr_1;
  arr_1.append (std::make_unique<string> ("foo"));
  // Self comparison should be 0
  ASSERT_JSON_EQ (arr_1, arr_1);

  // but should be different to an empty array:
  ASSERT_JSON_NE (arr_1, empty_arr_a);

  // Another with one element:
  array arr_2;
  arr_2.append (std::make_unique<string> ("foo"));
  ASSERT_JSON_EQ (arr_1, arr_2);

  // Adding an extra element:
  arr_2.append (std::make_unique<string> ("bar"));
  ASSERT_JSON_NE (arr_1, arr_2);
}

/* Run all of the selftests within this file.  */

void
json_cc_tests ()
{
  test_object_get ();
  test_writing_objects ();
  test_writing_arrays ();
  test_writing_float_numbers ();
  test_writing_integer_numbers ();
  test_writing_strings ();
  test_writing_literals ();
  test_formatting ();
  test_comparisons ();
}

} // namespace selftest

#endif /* #if CHECKING_P */
