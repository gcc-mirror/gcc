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

#define INCLUDE_MAP
#define INCLUDE_STRING
#define INCLUDE_VECTOR
#define INCLUDE_TYPE_TRAITS
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "diagnostic-core.h"
#include "json-parsing.h"
#include "json-diagnostic.h"
#include "aarch64-json-schema.h"
#include "aarch64-json-tunings-parser.h"
#include "aarch64-protos.h"
#include "config/arm/aarch-common-protos.h"
#include "selftest.h"
#include "version.h"

#define WARNING_OPT (0)

#define PARSE_INTEGER_FIELD(ctxt, obj, key, member)                            \
  {                                                                            \
    const json::value *val = obj.get (key);                                    \
    if (val)                                                                   \
      member = extract_integer (ctxt, *val, WARNING_OPT);                      \
  }

#define PARSE_UNSIGNED_INTEGER_FIELD(ctxt, obj, key, member)                   \
  {                                                                            \
    const json::value *val = obj.get (key);                                    \
    if (val)                                                                   \
      member = extract_unsigned_integer (ctxt, *val, WARNING_OPT);             \
  }

#define PARSE_BOOLEAN_FIELD(ctxt, obj, key, member)                             \
  {                                                                            \
    const json::value *val = obj.get (key);                                    \
    if (val)                                                                   \
      member = extract_boolean (ctxt, *val, WARNING_OPT);                      \
  }

#define PARSE_STRING_FIELD(ctxt, obj, key, member)                             \
  {                                                                            \
    const json::value *val = obj.get (key);                                    \
    if (val)                                                                   \
      member = extract_string (ctxt, *val, WARNING_OPT);                       \
  }

#define PARSE_OBJECT(ctxt, obj, key, member, parse_func)                       \
  {                                                                            \
    const json::value *field_value = obj.get (key);                            \
    if (field_value)                                                           \
      if (auto *field_obj = dyn_cast<const json::object *> (field_value))      \
	parse_object_helper (ctxt, *field_obj, (member), (parse_func));        \
  }

#define PARSE_ARRAY_FIELD(ctxt, obj, key, member, parse_func)                  \
  {                                                                            \
    const json::value *field_value = obj.get (key);                            \
    if (field_value)                                                           \
      if (auto *field_array = dyn_cast<const json::array *> (field_value))     \
	for (size_t i = 0; i < field_array->size (); ++i)                      \
	  {                                                                    \
	    const json::value *elem = field_array->get (i);                    \
	    if (elem)                                                          \
	      if (auto *array_obj = dyn_cast<const json::object *> (elem))     \
		parse_func (ctxt, *array_obj, member[i]);                      \
	  }                                                                    \
  }

#define PARSE_ENUM_FIELD(ctxt, obj, key, member, mappings)                     \
  parse_enum_field (ctxt, obj, key, member, mappings,                          \
		    sizeof (mappings) / sizeof (mappings[0]), WARNING_OPT)

/* Type alias for parse function pointer.  */
template <typename T>
using parse_func_type
  = void (*) (gcc_json_context &,
	      const json::object &,
	      std::remove_const_t<std::remove_pointer_t<T>> &);

/* Parse JSON object into non-pointer member type.  */
template <typename T>
static std::enable_if_t<!std::is_pointer<T>::value>
parse_object_helper (gcc_json_context &ctxt,
		     const json::object &field_obj, T &member,
		     parse_func_type<T> parse_func)
{
  parse_func (ctxt, field_obj, member);
}

/* Parse JSON object into a const pointer member by creating a temp copy.  */
template <typename T>
static std::enable_if_t<std::is_pointer<T>::value
			&& std::is_const<std::remove_pointer_t<T>>::value>
parse_object_helper (gcc_json_context &ctxt,
		     const json::object &field_obj, T &member,
		     parse_func_type<T> parse_func)
{
  /* Use static storage for the non-const copy.
     This works because tune_params does not have nested structures of the
     same type, but has room for errors if we end up having pointers to the
     same structure at some point.  */
  static bool already_initialized = false;
  if (already_initialized)
    {
      json_error (ctxt, field_obj,
		  "static storage conflict - multiple pointer members of "
		  "the same type cannot be parsed");
      return;
    }
  already_initialized = true;
  using NonConstType = std::remove_const_t<std::remove_pointer_t<T>>;
  if (!member)
    warning (0, "JSON tuning overrides an unspecified structure in the base "
		"tuning; fields not provided in JSON will default to 0");
  static NonConstType new_obj = member ? *member : NonConstType{};
  parse_func (ctxt, field_obj, new_obj);
  member = &new_obj;
}

/* Issue a note about the kind of json value we encountered.  */

static void
inform_about_wrong_kind_of_json_value (gcc_json_context &ctxt,
				       const json::value &val)
{
  switch (val.get_kind ())
    {
    default:
      gcc_unreachable ();
    case json::JSON_OBJECT:
      json_note (ctxt, val, "...but got an object instead");
      break;
    case json::JSON_ARRAY:
      json_note (ctxt, val, "...but got an array instead");
      break;
    case json::JSON_INTEGER:
      json_note (ctxt, val, "...but got an integer instead");
      break;
    case json::JSON_FLOAT:
      json_note (ctxt, val, "...but got a floating-point value instead");
      break;
    case json::JSON_STRING:
      json_note (ctxt, val, "...but got a string instead");
      break;
    case json::JSON_TRUE:
      json_note (ctxt, val, "...but got %qs instead", "true");
      break;
    case json::JSON_FALSE:
      json_note (ctxt, val, "...but got %qs instead", "false");
      break;
    case json::JSON_NULL:
      json_note (ctxt, val, "...but got %qs instead", "null");
      break;
    }
}

/* Extract string value from JSON, returning allocated C string.  */
char *
extract_string (gcc_json_context &ctxt,
		const json::value &val,
		diagnostics::option_id warning_opt)
{
  if (auto *string_val = dyn_cast<const json::string *> (&val))
    return xstrdup (string_val->get_string ());
  auto_diagnostic_group d;
  if (json_warning (ctxt, val, warning_opt, "expected a string..."))
    inform_about_wrong_kind_of_json_value (ctxt, val);
  return nullptr;
}

/* Extract signed integer value from JSON.  */
int
extract_integer (gcc_json_context &ctxt,
		 const json::value &val,
		 diagnostics::option_id warning_opt)
{
  if (auto *int_val = dyn_cast<const json::integer_number *> (&val))
    {
      long value = int_val->get ();
      gcc_assert (value >= INT_MIN && value <= INT_MAX);
      return static_cast<int> (value);
    }
  auto_diagnostic_group d;
  if (json_warning (ctxt, val, warning_opt, "expected an integer value..."))
    inform_about_wrong_kind_of_json_value (ctxt, val);
  return 0;
}

/* Extract unsigned integer value from JSON.  */
unsigned int
extract_unsigned_integer (gcc_json_context &ctxt,
			  const json::value &val,
			  diagnostics::option_id warning_opt)
{
  if (auto *int_val = dyn_cast<const json::integer_number *> (&val))
    {
      long value = int_val->get ();
      gcc_assert (value >= 0 && value <= UINT_MAX);
      return static_cast<unsigned int> (value);
    }
  auto_diagnostic_group d;
  if (json_warning (ctxt, val, warning_opt,
		    "expected an unsigned integer value..."))
    inform_about_wrong_kind_of_json_value (ctxt, val);
  return 0;
}

/* Extract boolean value from JSON literal.  */
bool
extract_boolean (gcc_json_context &ctxt,
		 const json::value &val,
		 diagnostics::option_id warning_opt)
{
  if (auto *literal_val = dyn_cast<const json::literal *> (&val))
    {
      json::kind kind = literal_val->get_kind ();
      if (kind == json::JSON_TRUE || kind == json::JSON_FALSE)
	return (kind == json::JSON_TRUE);
    }
  auto_diagnostic_group d;
  if (json_warning (ctxt, val, warning_opt, "expected a boolean value..."))
    inform_about_wrong_kind_of_json_value (ctxt, val);
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
parse_enum_field (gcc_json_context &ctxt,
		  const json::object &jo, const std::string &key,
		  EnumType &enum_var, const enum_mapping<EnumType> *mappings,
		  size_t num_mappings,
		  diagnostics::option_id warning_opt)
{
  const json::value *field_value = jo.get (key.c_str ());
  if (!field_value)
    return;

  auto *string_val = dyn_cast<const json::string *> (field_value);
  if (!string_val)
    {
      json_warning (ctxt, *field_value, warning_opt,
		    "expected string for enum field %s", key.c_str ());
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

  json_warning (ctxt, *field_value, 0,
		"%s not recognized, defaulting to %qs", key.c_str (),
		mappings[0].name);
  enum_var = mappings[0].value;
}

/* Include auto-generated parsing routines.  */
#include "aarch64-json-tunings-parser-generated.inc"

/* Validate the user provided JSON data against the present schema.
   Checks for correct types, fields, and expected format.  */
static bool
validate_and_traverse (gcc_json_context &ctxt,
		       const json::object *json_obj,
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
	  json_warning (ctxt, *json_value, WARNING_OPT,
			"key %qs is not a tuning parameter, skipping",
			full_key.c_str ());
	  continue;
	}

      if (auto *sub_schema_obj = dyn_cast<const json::object *> (schema_value))
	{
	  if (auto *sub_json_obj = dyn_cast<const json::object *> (json_value))
	    {
	      if (!validate_and_traverse (ctxt, sub_json_obj, sub_schema_obj,
					  full_key))
		return false;
	    }
	  else
	    {
	      json_error (ctxt, *json_value,
			  "key %qs expected to be an object",
			  full_key.c_str ());
	      return false;
	    }
	}
      else if (schema_value->get_kind () == json::JSON_ARRAY)
	{
	  if (json_value->get_kind () != json::JSON_ARRAY)
	    {
	      json_error (ctxt, *json_value,
			  "key %qs expected to be an array", full_key.c_str ());
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
		  json_error (ctxt, *json_value,
			      "key %qs expected to be an integer",
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
		      json_error (ctxt, *json_value,
				  "key %qs value %ld is out of range for "
				  "%<int%> type [%d, %d]",
				  full_key.c_str (), value, INT_MIN, INT_MAX);
		      return false;
		    }
		}
	    }
	  else if (strcmp (schema_type_str, "uint") == 0)
	    {
	      if (json_value->get_kind () != json::JSON_INTEGER)
		{
		  json_error (ctxt, *json_value,
			      "key %qs expected to be an unsigned integer",
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
		      json_error (ctxt, *json_value,
				  "key %qs value %ld is out of range for "
				  "%<uint%> type [0, %u]",
				  full_key.c_str (), value, UINT_MAX);
		      return false;
		    }
		}
	    }
	  else if (strcmp (schema_type_str, "string") == 0)
	    {
	      if (json_value->get_kind () != json::JSON_STRING)
		{
		  json_error (ctxt, *json_value,
			      "key %qs expected to be a string",
			      full_key.c_str ());
		  return false;
		}
	    }
	  else if (strcmp (schema_type_str, "boolean") == 0)
	    {
	      if (json_value->get_kind () != json::JSON_TRUE
		  && json_value->get_kind () != json::JSON_FALSE)
		{
		  json_error (ctxt, *json_value,
			      "key %qs expected to be a boolean (true/false)",
			      full_key.c_str ());
		  return false;
		}
	    }
	  else if (strcmp (schema_type_str, "enum") == 0)
	    {
	      if (json_value->get_kind () != json::JSON_STRING)
		{
		  json_error (ctxt, *json_value,
			      "key %qs expected to be an enum (string)",
			      full_key.c_str ());
		  return false;
		}
	    }
	  else
	    {
	      json_error (ctxt, *json_value,
			  "key %qs has unsupported type", full_key.c_str ());
	      return false;
	    }
	}
      else
	{
	  json_error (ctxt, *json_value,
		      "key %qs has unexpected format in schema",
		      full_key.c_str ());
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
      warning (WARNING_OPT,
	       "JSON tuning file does not contain version information; "
	       "compatibility cannot be verified");
      return true;
    }

  if (json_gcc_major_version != GCC_major_version)
    {
      auto_diagnostic_group d;
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
aarch64_load_tuning_params_from_json_string (const char *js_filename,
					     const char *json_string,
					     const char *schema_string,
					     struct tune_params *tune)
{
  /* Try parsing the JSON string.  */
  gcc_json_context ctxt (js_filename);
  json::parser_result_t data_result
    = json::parse_utf8_string (strlen (json_string), json_string, true,
			       &ctxt);

  if (auto json_err = data_result.m_err.get ())
    {
      location_t js_loc = ctxt.make_location_for_range (json_err->get_range ());
      error_at (js_loc, "error parsing JSON data: %s", json_err->get_msg ());
      return;
    }

  const json::value* root = data_result.m_val.get ();
  gcc_assert (root);

  auto *root_obj = dyn_cast<const json::object *> (root);
  if (!root_obj)
    {
      json_warning (ctxt, *root, WARNING_OPT,
		    "no JSON object found in the provided data");
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
      json_warning (ctxt, *root_obj, WARNING_OPT,
		    "key %<tune_params%> not found in JSON data");
      return;
    }

  auto *jo = dyn_cast<const json::object *> (tune_params_value);
  if (!jo)
    {
      json_error (ctxt, *tune_params_value,
		  "key %<tune_params%> is not a JSON object");
      return;
    }

  if (!validate_and_traverse (ctxt, root_obj, schema_obj))
    {
      error ("validation failed for the provided JSON data");
      return;
    }

  parse_tunings (ctxt, *jo, *tune);

  /* dispatch_constraints are not represented in JSON tunings.  If the JSON
     sets DISPATCH_SCHED in extra_tuning_flags but the base model does not
     provide dispatch_constraints, clear the flag to avoid an assertion
     failure later.  */
  if ((tune->extra_tuning_flags & AARCH64_EXTRA_TUNE_DISPATCH_SCHED)
      && tune->dispatch_constraints == nullptr)
    {
      warning (0, "JSON tuning enables dispatch scheduling but "
	       "%<dispatch_constraints%> is not available; "
	       "disabling dispatch scheduling");
      tune->extra_tuning_flags &= ~AARCH64_EXTRA_TUNE_DISPATCH_SCHED;
    }

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
  aarch64_load_tuning_params_from_json_string
    (data_filename,
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

  aarch64_load_tuning_params_from_json_string
    ("test.json", test_json, schema_json, &params);

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

  aarch64_load_tuning_params_from_json_string
    ("test.json", test_json, schema_json, &params);

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

  aarch64_load_tuning_params_from_json_string
    ("test.json", test_json, schema_json, &params);

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

  aarch64_load_tuning_params_from_json_string
    ("test.json", test_json, schema_json, &params);

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
