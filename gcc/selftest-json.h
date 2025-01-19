/* Selftest support for JSON.
   Copyright (C) 2024-2025 Free Software Foundation, Inc.
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

#ifndef GCC_SELFTEST_JSON_H
#define GCC_SELFTEST_JSON_H

#include "json.h"

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
		       const char *expected_value);
#define ASSERT_JSON_STRING_EQ(JSON_VALUE, EXPECTED_VALUE) \
  assert_json_string_eq ((SELFTEST_LOCATION),			\
			 (JSON_VALUE),				\
			 (EXPECTED_VALUE))

/* Assert that VALUE is a non-null json::object,
   returning it as such, failing at LOC if this isn't the case.  */

const json::object *
expect_json_object (const location &loc,
		    const json::value *value);

/* Assert that VALUE is a non-null json::object that has property
   PROPERTY_NAME.
   Return the value of the property.
   Use LOC for any failures.  */

const json::value *
expect_json_object_with_property (const location &loc,
				  const json::value *value,
				  const char *property_name);

/* Assert that VALUE is a non-null json::object that has property
   PROPERTY_NAME, and that the value of that property is a non-null
   json::integer_number equalling EXPECTED_VALUE.
   Use LOC for any failures.  */

void
assert_json_int_property_eq (const location &loc,
			     const json::value *value,
			     const char *property_name,
			     long expected_value);
#define ASSERT_JSON_INT_PROPERTY_EQ(JSON_VALUE, PROPERTY_NAME, EXPECTED_VALUE) \
  assert_json_int_property_eq ((SELFTEST_LOCATION),			\
			       (JSON_VALUE),		\
			       (PROPERTY_NAME),	\
			       (EXPECTED_VALUE))

/* Assert that VALUE is a non-null json::object that has property
   PROPERTY_NAME, and that the property value is a non-null JSON object.
   Return the value of the property as a json::object.
   Use LOC for any failures.  */

const json::object *
expect_json_object_with_object_property (const location &loc,
					 const json::value *value,
					 const char *property_name);
#define EXPECT_JSON_OBJECT_WITH_OBJECT_PROPERTY(JSON_VALUE, PROPERTY_NAME) \
  expect_json_object_with_object_property ((SELFTEST_LOCATION),		\
					   (JSON_VALUE),		\
					   (PROPERTY_NAME))

/* Assert that VALUE is a non-null json::object that has property
   PROPERTY_NAME, and that the property value is a non-null JSON array.
   Return the value of the property as a json::array.
   Use LOC for any failures.  */

const json::array *
expect_json_object_with_array_property (const location &loc,
					const json::value *value,
					const char *property_name);
#define EXPECT_JSON_OBJECT_WITH_ARRAY_PROPERTY(JSON_VALUE, PROPERTY_NAME) \
  expect_json_object_with_array_property ((SELFTEST_LOCATION),		\
					  (JSON_VALUE),		\
					  (PROPERTY_NAME))

/* Assert that VALUE is a non-null json::object that has property
   PROPERTY_NAME, and that the property value is a non-null JSON string.
   Return the value of the property as a json::string.
   Use LOC for any failures.  */

const json::string *
expect_json_object_with_string_property (const location &loc,
					 const json::value *value,
					 const char *property_name);
#define EXPECT_JSON_OBJECT_WITH_STRING_PROPERTY(JSON_VALUE, PROPERTY_NAME) \
  expect_json_object_with_string_property ((SELFTEST_LOCATION),		\
					   (JSON_VALUE),		\
					   (PROPERTY_NAME))

/* Assert that VALUE is a non-null json::object that has property
   PROPERTY_NAME, and that the value of that property is a non-null
   JSON string equalling EXPECTED_VALUE.
   Use LOC for any failures.  */

void
assert_json_string_property_eq (const location &loc,
				const json::value *value,
				const char *property_name,
				const char *expected_value);
#define ASSERT_JSON_STRING_PROPERTY_EQ(JSON_VALUE, PROPERTY_NAME, EXPECTED_VALUE) \
  assert_json_string_property_eq ((SELFTEST_LOCATION),			\
				  (JSON_VALUE),				\
				  (PROPERTY_NAME),			\
				  (EXPECTED_VALUE))

} // namespace selftest

#endif /* #if CHECKING_P */

#endif /* GCC_SELFTEST_JSON_H */
