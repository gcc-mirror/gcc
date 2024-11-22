/* Selftest support for JSON.
   Copyright (C) 2024 Free Software Foundation, Inc.
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
#include "diagnostic.h"
#include "selftest.h"
#include "selftest-json.h"

/* The selftest code should entirely disappear in a production
   configuration, hence we guard all of it with #if CHECKING_P.  */

#if CHECKING_P

namespace selftest {

/* Assert that VALUE is a non-null json::string
   equalling EXPECTED_VALUE.
   Use LOC for any failures.  */

void
assert_json_string_eq (const location &loc,
		       const json::value *value,
		       const char *expected_value)
{
  ASSERT_EQ_AT (loc, value->get_kind (), json::JSON_STRING);
  const json::string *str = static_cast<const json::string *> (value);
  ASSERT_STREQ_AT (loc, expected_value, str->get_string ());
}

/* Assert that VALUE is a non-null json::object,
   returning it as such, failing at LOC if this isn't the case.  */

const json::object *
expect_json_object (const location &loc,
		    const json::value *value)
{
  ASSERT_NE_AT (loc, value, nullptr);
  ASSERT_EQ_AT (loc, value->get_kind (), json::JSON_OBJECT);
  return static_cast<const json::object *> (value);
}

/* Assert that VALUE is a non-null json::object that has property
   PROPERTY_NAME.
   Return the value of the property.
   Use LOC for any failures.  */

const json::value *
expect_json_object_with_property (const location &loc,
				  const json::value *value,
				  const char *property_name)
{
  const json::object *obj = expect_json_object (loc, value);
  const json::value *property_value = obj->get (property_name);
  ASSERT_NE_AT (loc, property_value, nullptr);
  return property_value;
}

/* Assert that VALUE is a non-null json::object that has property
   PROPERTY_NAME, and that the value of that property is a non-null
   json::integer_number equalling EXPECTED_VALUE.
   Use LOC for any failures.  */

void
assert_json_int_property_eq (const location &loc,
			     const json::value *value,
			     const char *property_name,
			     long expected_value)
{
  const json::value *property_value
    = expect_json_object_with_property (loc, value, property_name);
  ASSERT_EQ_AT (loc, property_value->get_kind (), json::JSON_INTEGER);
  long actual_value
    = static_cast<const json::integer_number *> (property_value)->get ();
  ASSERT_EQ_AT (loc, expected_value, actual_value);
}

/* Assert that VALUE is a non-null json::object that has property
   PROPERTY_NAME, and that the property value is a non-null JSON object.
   Return the value of the property as a json::object.
   Use LOC for any failures.  */

const json::object *
expect_json_object_with_object_property (const location &loc,
					 const json::value *value,
					 const char *property_name)
{
  const json::value *property_value
    = expect_json_object_with_property (loc, value, property_name);
  ASSERT_EQ_AT (loc, property_value->get_kind (), json::JSON_OBJECT);
  return static_cast<const json::object *> (property_value);
}

/* Assert that VALUE is a non-null json::object that has property
   PROPERTY_NAME, and that the property value is a non-null JSON array.
   Return the value of the property as a json::array.
   Use LOC for any failures.  */

const json::array *
expect_json_object_with_array_property (const location &loc,
					const json::value *value,
					const char *property_name)
{
  const json::value *property_value
    = expect_json_object_with_property (loc, value, property_name);
  ASSERT_EQ_AT (loc, property_value->get_kind (), json::JSON_ARRAY);
  return static_cast<const json::array *> (property_value);
}

/* Assert that VALUE is a non-null json::object that has property
   PROPERTY_NAME, and that the property value is a non-null JSON string.
   Return the value of the property as a json::string.
   Use LOC for any failures.  */

const json::string *
expect_json_object_with_string_property (const location &loc,
					 const json::value *value,
					 const char *property_name)
{
  const json::value *property_value
    = expect_json_object_with_property (loc, value, property_name);
  ASSERT_EQ_AT (loc, property_value->get_kind (), json::JSON_STRING);
  return static_cast<const json::string *> (property_value);
}

/* Assert that VALUE is a non-null json::object that has property
   PROPERTY_NAME, and that the value of that property is a non-null
   JSON string equalling EXPECTED_VALUE.
   Use LOC for any failures.  */

void
assert_json_string_property_eq (const location &loc,
				const json::value *value,
				const char *property_name,
				const char *expected_value)
{
  const json::value *property_value
    = expect_json_object_with_property (loc, value, property_name);
  assert_json_string_eq (loc, property_value, expected_value);
}

} // namespace selftest

#endif /* #if CHECKING_P */
