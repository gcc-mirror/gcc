/* Routines to print the AArch64 tuning parameters to a JSON file.
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

#define INCLUDE_TYPE_TRAITS
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "pretty-print.h"
#include "tm.h"
#include "diagnostic-core.h"
#include "aarch64-json-tunings-printer.h"
#include "aarch64-protos.h"
#include "config/arm/aarch-common-protos.h"
#include "json.h"
#include "version.h"

#define SERIALIZE_INTEGER_FIELD(obj, key, member)                              \
  (obj)->set_integer ((key), (member))

#define SERIALIZE_UNSIGNED_INTEGER_FIELD(obj, key, member)                     \
  (obj)->set_integer ((key), (member))

#define SERIALIZE_BOOLEAN_FIELD(obj, key, member)                              \
  (obj)->set_bool ((key), (member))

#define SERIALIZE_STRING_FIELD(obj, key, member)                               \
  (obj)->set_string ((key), (member))

#define SERIALIZE_OBJECT(obj, key, member, serialize_func)                     \
  {                                                                            \
    auto field_obj = serialize_object_helper ((member), (serialize_func));     \
    if (field_obj)                                                             \
      (obj)->set ((key), std::move (field_obj));                               \
  }

#define SERIALIZE_ARRAY_FIELD(obj, key, member, size, serialize_func)          \
  {                                                                            \
    auto field_array = std::make_unique<json::array> ();                       \
    for (size_t i = 0; i < (size); ++i)                                        \
      {                                                                        \
	auto element_obj = serialize_func ((member)[i]);                       \
	if (element_obj)                                                       \
	  field_array->append (std::move (element_obj));                       \
      }                                                                        \
    (obj)->set ((key), std::move (field_array));                               \
  }

#define SERIALIZE_ENUM_FIELD(obj, key, member, mappings)                       \
  (obj)->set_string ((key), serialize_enum ((member), (mappings),              \
					    sizeof (mappings)                  \
					      / sizeof (mappings[0])))

/* Type alias for serialize function pointer.  */
template <typename T>
using serialize_func_type = std::unique_ptr<json::object> (*) (
  const typename std::remove_pointer<T>::type &);

/* Serialize JSON object from non-pointer members.  */
template <typename T>
static typename std::enable_if<!std::is_pointer<T>::value,
			       std::unique_ptr<json::object>>::type
serialize_object_helper (const T &member, serialize_func_type<T> serialize_func)
{
  return serialize_func (member);
}

/* Serialize JSON object from pointer members.  */
template <typename T>
static typename std::enable_if<std::is_pointer<T>::value,
			       std::unique_ptr<json::object>>::type
serialize_object_helper (const T &member, serialize_func_type<T> serialize_func)
{
  if (member)
    return serialize_func (*member);
  return std::make_unique<json::object> ();
}

/* Mapping structure for enum-to-string conversion.  */
template <typename EnumType> struct enum_mapping
{
  const char *name;
  EnumType value;
};

/* Convert enum value to string using enum-to-string mappings.  */
template <typename EnumType>
static const char *
serialize_enum (EnumType enum_value, const enum_mapping<EnumType> *mappings,
		size_t num_mappings)
{
  for (size_t i = 0; i < num_mappings; ++i)
    if (enum_value == mappings[i].value)
      return mappings[i].name;
  return mappings[0].name;
}

/* Include auto-generated printing routines.  */
#include "aarch64-json-tunings-printer-generated.inc"

/* Print tune_params structure to JSON file.  */
void
aarch64_print_tune_params (const tune_params &params, const char *filename)
{
  /* Use default filename if none provided or empty string given.  */
  const char *output_filename = filename;
  if (!output_filename || *output_filename == '\0')
    output_filename = "aarch64-tuning.json";

  auto aarch64_tune_params_json = std::make_unique<json::object> ();

  auto metadata = std::make_unique<json::object> ();
  metadata->set_integer ("gcc_version", GCC_major_version);
  aarch64_tune_params_json->set ("metadata", std::move (metadata));

  aarch64_tune_params_json->set ("tune_params", serialize_tunings (params));

  pretty_printer pp;
  aarch64_tune_params_json->print (&pp, true);

  FILE *outputFile = fopen (output_filename, "w");
  if (!outputFile)
    {
      error ("Error opening file %s", output_filename);
      return;
    }

  fprintf (outputFile, "%s", pp_formatted_text (&pp));
  fclose (outputFile);
  return;
}