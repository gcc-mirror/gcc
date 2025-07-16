/* Routines to parse the AArch64 tuning parameters from a JSON file.
   Copyright The GNU Toolchain Authors.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define INCLUDE_STRING
#define INCLUDE_VECTOR
#define INCLUDE_TYPE_TRAITS
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "diagnostic-core.h"
#include "json-parsing.h"
#include "aarch64-json-schema.h"
#include "aarch64-json-tunings-parser.h"
#include "aarch64-protos.h"
#include "config/arm/aarch-common-protos.h"
#include "selftest.h"
#include "version.h"

#define PARSE_INTEGER_FIELD(obj, key, member)                                  \
  {                                                                            \
    const json::value *val = obj->get (key);                                   \
    if (val)                                                                   \
      member = extract_integer (val);                                          \
  }

#define PARSE_UNSIGNED_INTEGER_FIELD(obj, key, member)                         \
  {                                                                            \
    const json::value *val = obj->get (key);                                   \
    if (val)                                                                   \
      member = extract_unsigned_integer (val);                                 \
  }

#define PARSE_BOOLEAN_FIELD(obj, key, member)                                  \
  {                                                                            \
    const json::value *val = obj->get (key);                                   \
    if (val)                                                                   \
      member = extract_boolean (val);                                          \
  }

#define PARSE_STRING_FIELD(obj, key, member)                                   \
  {                                                                            \
    const json::value *val = obj->get (key);                                   \
    if (val)                                                                   \
      member = extract_string (val);                                           \
  }

#define PARSE_OBJECT(obj, key, member, parse_func)                             \
  {                                                                            \
    const json::value *field_value = obj->get (key);                           \
    if (field_value)                                                           \
      if (auto *field_obj = dyn_cast<const json::object *> (field_value))      \
	parse_object_helper (field_obj, (member), (parse_func));               \
  }

#define PARSE_ARRAY_FIELD(obj, key, member, parse_func)                        \
  {                                                                            \
    const json::value *field_value = obj->get (key);                           \
    if (field_value)                                                           \
      if (auto *field_array = dyn_cast<const json::array *> (field_value))     \
	for (size_t i = 0; i < field_array->size (); ++i)                      \
	  {                                                                    \
	    const json::value *elem = field_array->get (i);                    \
	    if (elem)                                                          \
	      if (auto *array_obj = dyn_cast<const json::object *> (elem))     \
		parse_func (array_obj, member[i]);                             \
	  }                                                                    \
  }

#define PARSE_ENUM_FIELD(obj, key, member, mappings)                           \
  parse_enum_field (obj, key, member, mappings,                                \
		    sizeof (mappings) / sizeof (mappings[0]))

/* Type alias for parse function pointer.  */
template <typename T>
using parse_func_type
  = void (*) (const json::object *,
	      std::remove_const_t<std::remove_pointer_t<T>> &);

/* Parse JSON object into non-pointer member type.  */
template <typename T>
static std::enable_if_t<!std::is_pointer<T>::value>
parse_object_helper (const json::object *field_obj, T &member,
		     parse_func_type<T> parse_func)
{
  parse_func (field_obj, member);
}

/* Parse JSON object into a const pointer member by creating a temp copy.  */
template <typename T>
static std::enable_if_t<std::is_pointer<T>::value
			&& std::is_const<std::remove_pointer_t<T>>::value>
parse_object_helper (const json::object *field_obj, T &member,
		     parse_func_type<T> parse_func)
{
  if (!member)
    return;

  /* Use static storage for the non-const copy.
     This works because tune_params does not have nested structures of the
     same type, but has room for errors if we end up having pointers to the
     same structure at some point.  */
  static bool already_initialized = false;
  if (already_initialized)
    {
      error ("static storage conflict - multiple pointer members of the "
	     "same type cannot be parsed");
      return;
    }
  already_initialized = true;
  using NonConstType = std::remove_const_t<std::remove_pointer_t<T>>;
  static NonConstType new_obj = *member;
  parse_func (field_obj, new_obj);
  member = &new_obj;
}

/* Extract string value from JSON, returning allocated C string.  */
char *
extract_string (const json::value *val)
{
  if (auto *string_val = dyn_cast<const json::string *> (val))
    return xstrdup (string_val->get_string ());
  warning (0, "expected a string but got something else or NULL");
  return nullptr;
}

/* Extract signed integer value from JSON.  */
int
extract_integer (const json::value *val)
{
  if (auto *int_val = dyn_cast<const json::integer_number *> (val))
    {
      long value = int_val->get ();
      gcc_assert (value >= INT_MIN && value <= INT_MAX);
      return static_cast<int> (value);
    }
  warning (0, "expected an integer value but got something else or NULL");
  return 0;
}

/* Extract unsigned integer value from JSON.  */
unsigned int
extract_unsigned_integer (const json::value *val)
{
  if (auto *int_val = dyn_cast<const json::integer_number *> (val))
    {
      long value = int_val->get ();
      gcc_assert (value >= 0 && value <= UINT_MAX);
      return static_cast<unsigned int> (value);
    }
  warning (0,
	   "expected an unsigned integer value but got something else or NULL");
  return 0;
}

/* Extract boolean value from JSON literal.  */
bool
extract_boolean (const json::value *val)
{
  if (auto *literal_val = dyn_cast<const json::literal *> (val))
    {
      json::kind kind = literal_val->get_kind ();
      if (kind == json::JSON_TRUE || kind == json::JSON_FALSE)
	return (kind == json::JSON_TRUE);
    }
  warning (0, "expected a boolean value but got something else or NULL");
  return false;
}

template <typename EnumType> struct enum_mapping
{
  const char *name;
  EnumType value;
};

/* Parse JSON string field into enum value using string-to-enum mappings.  */
template <typename EnumType>
static void
parse_enum_field (const json::object *jo, const std::string &key,
		  EnumType &enum_var, const enum_mapping<EnumType> *mappings,
		  size_t num_mappings)
{
  const json::value *field_value = jo->get (key.c_str ());
  if (!field_value)
    return;

  auto *string_val = dyn_cast<const json::string *> (field_value);
  if (!string_val)
    {
      warning (0, "expected string for enum field %s", key.c_str ());
      enum_var = mappings[0].value;
      return;
    }

  const char *field_string = string_val->get_string ();
  for (size_t i = 0; i < num_mappings; ++i)
    {
      if (strcmp (field_string, mappings[i].name) == 0)
	{
	  enum_var = mappings[i].value;
	  return;
	}
    }

  warning (0, "%s not recognized, defaulting to %qs", key.c_str (),
	   mappings[0].name);
  enum_var = mappings[0].value;
}

/* Include auto-generated parsing routines.  */
#include "aarch64-json-tunings-parser-generated.inc"

/* Validate the user provided JSON data against the present schema.
   Checks for correct types, fields, and expected format.  */
static bool
validate_and_traverse (const json::object *json_obj,
		       const json::object *schema_obj,
		       const std::string &parent_key = "")
{
  for (const auto &json_entry : json_obj->get_map ())
    {
      const std::string &key = json_entry.first;
      const json::value *json_value = json_entry.second;

      std::string full_key = parent_key.empty () ? key : parent_key + "." + key;

      const json::value *schema_value = schema_obj->get (key.c_str ());
      if (!schema_value)
	{
	  warning (0, "key %qs is not a tuning parameter, skipping",
		   full_key.c_str ());
	  continue;
	}

      if (auto *sub_schema_obj = dyn_cast<const json::object *> (schema_value))
	{
	  if (auto *sub_json_obj = dyn_cast<const json::object *> (json_value))
	    {
	      if (!validate_and_traverse (sub_json_obj, sub_schema_obj,
					  full_key))
		return false;
	    }
	  else
	    {
	      error ("key %qs expected to be an object", full_key.c_str ());
	      return false;
	    }
	}
      else if (schema_value->get_kind () == json::JSON_ARRAY)
	{
	  if (json_value->get_kind () != json::JSON_ARRAY)
	    {
	      error ("key %qs expected to be an array", full_key.c_str ());
	      return false;
	    }
	}
      else if (auto *schema_string
	       = dyn_cast<const json::string *> (schema_value))
	{
	  const char *schema_type_str = schema_string->get_string ();

	  if (strcmp (schema_type_str, "int") == 0)
	    {
	      if (json_value->get_kind () != json::JSON_INTEGER)
		{
		  error ("key %qs expected to be an integer",
			 full_key.c_str ());
		  return false;
		}
	      // Check if the value is valid for signed integer
	      if (auto *int_val
		  = dyn_cast<const json::integer_number *> (json_value))
		{
		  long value = int_val->get ();
		  if (value > INT_MAX || value < INT_MIN)
		    {
		      error ("key %qs value %ld is out of range for %<int%> "
			     "type [%d, %d]",
			     full_key.c_str (), value, INT_MIN, INT_MAX);
		      return false;
		    }
		}
	    }
	  else if (strcmp (schema_type_str, "uint") == 0)
	    {
	      if (json_value->get_kind () != json::JSON_INTEGER)
		{
		  error ("key %qs expected to be an unsigned integer",
			 full_key.c_str ());
		  return false;
		}
	      // Check if the value is valid for unsigned integer
	      if (auto *int_val
		  = dyn_cast<const json::integer_number *> (json_value))
		{
		  long value = int_val->get ();
		  if (value < 0 || value > UINT_MAX)
		    {
		      error ("key %qs value %ld is out of range for %<uint%> "
			     "type [0, %u]",
			     full_key.c_str (), value, UINT_MAX);
		      return false;
		    }
		}
	    }
	  else if (strcmp (schema_type_str, "string") == 0)
	    {
	      if (json_value->get_kind () != json::JSON_STRING)
		{
		  error ("key %qs expected to be a string", full_key.c_str ());
		  return false;
		}
	    }
	  else if (strcmp (schema_type_str, "boolean") == 0)
	    {
	      if (json_value->get_kind () != json::JSON_TRUE
		  && json_value->get_kind () != json::JSON_FALSE)
		{
		  error ("key %qs expected to be a boolean (true/false)",
			 full_key.c_str ());
		  return false;
		}
	    }
	  else if (strcmp (schema_type_str, "enum") == 0)
	    {
	      if (json_value->get_kind () != json::JSON_STRING)
		{
		  error ("key %qs expected to be an enum (string)",
			 full_key.c_str ());
		  return false;
		}
	    }
	  else
	    {
	      error ("key %qs has unsupported type", full_key.c_str ());
	      return false;
	    }
	}
      else
	{
	  error ("key %qs has unexpected format in schema", full_key.c_str ());
	  return false;
	}
    }
  return true;
}

/* Helper routine for reading the provided JSON file.  */
static std::unique_ptr<std::vector<char>>
read_file (const char *path)
{
  FILE *f_in = fopen (path, "r");
  if (!f_in)
    {
      error ("could not open file %s", path);
      return nullptr;
    }

  auto result = std::make_unique<std::vector<char>> ();
  char buf[4096];

  while (size_t iter_sz_in = fread (buf, 1, sizeof (buf), f_in))
    result->insert (result->end (), buf, buf + iter_sz_in);

  if (!feof (f_in))
    {
      error ("error reading file %s", path);
      fclose (f_in);
      return nullptr;
    }

  fclose (f_in);
  result->push_back ('\0');
  return result;
}

static bool
check_version_compatibility (const json::object *root_obj)
{
  const json::value *metadata_value = root_obj->get ("metadata");
  int json_gcc_major_version = -1;

  if (metadata_value)
    {
      if (auto *metadata_obj = dyn_cast<const json::object *> (metadata_value))
	{
	  const json::value *version_value = metadata_obj->get ("gcc_version");
	  if (version_value)
	    {
	      if (auto *version_int_val
		  = dyn_cast<const json::integer_number *> (version_value))
		json_gcc_major_version = version_int_val->get ();
	    }
	}
    }

  if (json_gcc_major_version == -1)
    {
      warning (0, "JSON tuning file does not contain version information; "
		  "compatibility cannot be verified");
      return true;
    }

  if (json_gcc_major_version != GCC_major_version)
    {
      error ("JSON tuning file was created with GCC version %d "
	     "but current GCC version is %d",
	     json_gcc_major_version, GCC_major_version);
      inform (UNKNOWN_LOCATION, "JSON tuning files must be regenerated "
				"when switching between major GCC versions");
      return false;
    }

  return true;
}

/* Main routine for setting up the parsing of JSON data.  */
static void
aarch64_load_tuning_params_from_json_string (const char *json_string,
					     const char *schema_string,
					     struct tune_params *tune)
{
  /* Try parsing the JSON string.  */
  json::parser_result_t data_result
    = json::parse_utf8_string (strlen (json_string), json_string, true,
			       nullptr);

  if (auto json_err = data_result.m_err.get ())
    {
      error ("error parsing JSON data: %s", json_err->get_msg ());
      return;
    }

  const std::unique_ptr<json::value> &root = data_result.m_val;
  if (!root)
    {
      error ("JSON parsing returned null data");
      return;
    }
  auto *root_obj = dyn_cast<const json::object *> (root.get ());
  if (!root_obj)
    {
      warning (0, "no JSON object found in the provided data");
      return;
    }

  /* Check version compatibility before proceeding.  */
  if (!check_version_compatibility (root_obj))
    return;

  json::parser_result_t schema_result
    = json::parse_utf8_string (strlen (schema_string), schema_string, true,
			       nullptr);

  gcc_assert (!schema_result.m_err.get ());
  gcc_assert (schema_result.m_val);

  auto *schema_obj
    = dyn_cast<const json::object *> (schema_result.m_val.get ());
  gcc_assert (schema_obj);

  const json::value *tune_params_value = root_obj->get ("tune_params");
  if (!tune_params_value)
    {
      warning (0, "key %<tune_params%> not found in JSON data");
      return;
    }

  auto *jo = dyn_cast<const json::object *> (tune_params_value);
  if (!jo)
    {
      error ("key %<tune_params%> is not a JSON object");
      return;
    }

  if (!validate_and_traverse (root_obj, schema_obj))
    {
      error ("validation failed for the provided JSON data");
      return;
    }

  parse_tunings (jo, *tune);
  return;
}

/* Wrapper for calling aarch64_load_tuning_params_from_json_string.  */
void
aarch64_load_tuning_params_from_json (const char *data_filename,
				      struct tune_params *tune)
{
  std::unique_ptr<std::vector<char>> json_data = read_file (data_filename);
  if (!json_data || !json_data->data ())
    {
      error ("cannot read JSON data in %s", data_filename);
      return;
    }
  aarch64_load_tuning_params_from_json_string (
    (const char *) json_data->data (), schema_json, tune);
}

#if CHECKING_P
namespace selftest {

#define STR_(X) #X
#define STR(X) STR_(X)

void
test_json_integers ()
{
  const char *test_json = R"json({
	"metadata": {
	  "gcc_version": )json" STR (GCC_major_version) R"json(
	},
	"tune_params": {
	  "sve_width": 256,
	  "issue_rate": 4
	}
    })json";

  tune_params params;

  aarch64_load_tuning_params_from_json_string (test_json, schema_json, &params);

  ASSERT_EQ (params.sve_width, 256);
  ASSERT_EQ (params.issue_rate, 4);
}

void
test_json_boolean ()
{
  const char *test_json = R"json({
	"metadata": {
	  "gcc_version": )json" STR (GCC_major_version) R"json(
	},
	"tune_params": {
	    "insn_extra_cost": {
		"alu": {
		    "non_exec_costs_exec": false
		}
	    }
	}
    })json";

  static const cpu_cost_table default_cost_table = {};

  tune_params params;
  params.insn_extra_cost = &default_cost_table;

  aarch64_load_tuning_params_from_json_string (test_json, schema_json, &params);

  ASSERT_EQ (params.insn_extra_cost->alu.non_exec_costs_exec, false);
}

void
test_json_strings ()
{
  const char *test_json = R"json({
	"metadata": {
	  "gcc_version": )json" STR (GCC_major_version) R"json(
	},
	"tune_params": {
	    "function_align": "16",
	    "jump_align": "2",
	    "loop_align": "8"
	}
    })json";

  tune_params params;

  aarch64_load_tuning_params_from_json_string (test_json, schema_json, &params);

  ASSERT_STREQ (params.function_align, "16");
  ASSERT_STREQ (params.jump_align, "2");
  ASSERT_STREQ (params.loop_align, "8");
}

void
test_json_enums ()
{
  const char *test_json = R"json({
	"metadata": {
	  "gcc_version": )json" STR (GCC_major_version) R"json(
	},
	"tune_params": {
	    "autoprefetcher_model": "AUTOPREFETCHER_OFF",
	    "ldp_policy_model": "AARCH64_LDP_STP_POLICY_NEVER",
	    "stp_policy_model": "AARCH64_LDP_STP_POLICY_DEFAULT"
	}
    })json";

  tune_params params;

  aarch64_load_tuning_params_from_json_string (test_json, schema_json, &params);

  ASSERT_EQ (params.autoprefetcher_model, tune_params::AUTOPREFETCHER_OFF);
  ASSERT_EQ (params.ldp_policy_model, AARCH64_LDP_STP_POLICY_NEVER);
  ASSERT_EQ (params.stp_policy_model, AARCH64_LDP_STP_POLICY_DEFAULT);
}

void
aarch64_json_tunings_tests ()
{
  test_json_integers ();
  test_json_boolean ();
  test_json_strings ();
  test_json_enums ();
}

} // namespace selftest

#undef STR
#undef STR_

#endif /* CHECKING_P */