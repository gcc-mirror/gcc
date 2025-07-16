#!/usr/bin/env python3

# Script to autogenerate the parsing and serialization routines for the
# aarch64 JSON tuning parameters.
#
# Copyright The GNU Toolchain Authors.
#
# This file is part of GCC.
#
# GCC is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 3, or (at your option) any later
# version.
#
# GCC is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

DESCRIPTION = """
Maintenance script to regenerate aarch64-json-tunings-*-generated.inc files
from the JSON schema in aarch64-json-schema.h.

This script is run automatically whenever aarch64-json-schema.h is modified.

Usage:

    python3 <path-to-script>/aarch64-generate-json-tuning-routines.py [options]

Options:
    --generate-only <parser|printer>  Generate only parser or printer file.
                                      If not specified, generates both.

Note that the script can be called from any directory.

Generates (in gcc/config/aarch64/):
  aarch64-json-tunings-parser-generated.inc
  aarch64-json-tunings-printer-generated.inc
"""

import json
import re
import os
import argparse
from typing import Dict, Any, List, Tuple

def extract_schema_from_header(file_path: str) -> str:
    with open(file_path, "r") as f:
        content = f.read()

    # Find the schema_json variable content between R"json( and )json"
    pattern = r'static const char \*schema_json = R"json\((.*?)\)json";'
    match = re.search(pattern, content, re.DOTALL)

    if not match:
        raise ValueError("Could not find schema_json in header file")

    return match.group(1).strip()

def get_macro(operation: str, field_type: str) -> str:
    type_map = {
        "int": "INTEGER",
        "uint": "UNSIGNED_INTEGER",
        "boolean": "BOOLEAN",
        "string": "STRING",
        "enum": "ENUM",
    }
    if field_type not in type_map:
        raise ValueError(f"Unknown field type: {field_type}")
    return f"{operation}_{type_map[field_type]}_FIELD"

def generate_field_code(
    operation: str,
    key: str,
    value: Any,
    struct_name: str,
    current_path: List[str],
    function_map: Dict[str, str],
    obj_name: str = "jo",
    indent: str = "  ",
) -> List[str]:
    lines = []

    if isinstance(value, str):
        macro = get_macro(operation.upper(), value)
        if value == "enum":
            enum_mapping = f"{key}_mappings"
            lines.append(
                f'{indent}{macro} ({obj_name}, "{key}", {struct_name}.{key}, {enum_mapping});'
            )
        else:
            lines.append(f'{indent}{macro} ({obj_name}, "{key}", {struct_name}.{key});')

    elif isinstance(value, dict):
        # Nested object - find function name based on current context + key
        child_path = current_path + [key]
        child_path_key = "_".join(child_path)
        func_name = function_map.get(child_path_key, f"{operation.lower()}_{key}")
        macro_name = f"{operation.upper()}_OBJECT"
        lines.append(
            f'{indent}{macro_name} ({obj_name}, "{key}", {struct_name}.{key}, {func_name});'
        )

    elif isinstance(value, list) and len(value) > 0:
        if isinstance(value[0], dict):
            element_key = f"{key}_element"
            element_path = current_path + [element_key]
            element_path_key = "_".join(element_path)
            func_name = function_map.get(
                element_path_key, f"{operation.lower()}_{element_key}"
            )
            macro_name = f"{operation.upper()}_ARRAY_FIELD"

            if operation.lower() == "serialize":
                lines.append(
                    f'{indent}{macro_name} ({obj_name}, "{key}", {struct_name}.{key}, ARRAY_SIZE ({struct_name}.{key}), {func_name});'
                )
            else:
                lines.append(
                    f'{indent}{macro_name} ({obj_name}, "{key}", {struct_name}.{key}, {func_name});'
                )
        else:
            raise ValueError(f"Arrays of non-object types are not yet supported: {key}")
    else:
        raise ValueError(f"Unhandled field type for key '{key}': {type(value)}")

    return lines

def generate_field_parsing(
    key: str,
    value: Any,
    struct_name: str,
    current_path: List[str],
    function_map: Dict[str, str],
    indent: str = "  ",
) -> List[str]:
    return generate_field_code(
        "parse", key, value, struct_name, current_path, function_map, "jo", indent
    )

def generate_field_serialization(
    key: str,
    value: Any,
    struct_name: str,
    obj_name: str,
    current_path: List[str],
    function_map: Dict[str, str],
    indent: str = "  ",
) -> List[str]:
    return generate_field_code(
        "serialize",
        key,
        value,
        struct_name,
        current_path,
        function_map,
        obj_name,
        indent,
    )

def generate_function(
    operation: str,
    full_name: str,
    local_name: str,
    schema: Dict[str, Any],
    current_path: List[str],
    function_map: Dict[str, str],
) -> List[str]:
    lines = []
    lines.append("template <typename T>")

    if operation.lower() == "parse":
        lines.append("static void")
        lines.append(f"parse_{full_name} (const json::object *jo, T &{local_name})")
        lines.append("{")

        for key, value in schema.items():
            field_lines = generate_field_parsing(
                key, value, local_name, current_path, function_map
            )
            lines.extend(field_lines)

    elif operation.lower() == "serialize":
        lines.append("static std::unique_ptr<json::object>")
        lines.append(f"serialize_{full_name} (const T &{local_name})")
        lines.append("{")
        lines.append(f"  auto {local_name}_obj = std::make_unique<json::object> ();")
        lines.append("")

        for key, value in schema.items():
            field_lines = generate_field_serialization(
                key, value, local_name, f"{local_name}_obj", current_path, function_map
            )
            lines.extend(field_lines)

        lines.append("")
        lines.append(f"  return {local_name}_obj;")

    lines.append("}")

    return lines

"""Collect all object schemas with their full paths. This is necessary for
generating names for the routines with the correct hierarchal path to ensure
that identical keys in different structures are not given the same name.
For example:
vec_costs.issue_info.sve maps to <parse/serialize>_vec_costs_issue_info_sve
vec_costs.sve maps to <parse/serialize>_vec_costs_sve.
"""
def collect_all_objects_with_paths(
    schema: Dict[str, Any], path: List[str] = []
) -> Dict[str, Tuple[List[str], Dict[str, Any]]]:
    objects = {}

    for key, value in schema.items():
        current_path = path + [key]

        if isinstance(value, dict):
            path_key = "_".join(current_path)
            objects[path_key] = (current_path, value)
            nested = collect_all_objects_with_paths(value, current_path)
            objects.update(nested)

        elif isinstance(value, list) and len(value) > 0 and isinstance(value[0], dict):
            element_key = key.rstrip("s") if key.endswith("s") else f"{key}_element"
            element_path = current_path[:-1] + [element_key]
            element_path_key = "_".join(element_path)
            objects[element_path_key] = (element_path, value[0])
            nested = collect_all_objects_with_paths(value[0], element_path)
            objects.update(nested)

    return objects

"""Calculate dependency depth of an object schema. 0 indicates no
dependencies, ie. the object has only primitive types."""
def get_dependency_depth(obj_schema: Dict[str, Any]) -> int:
    max_depth = 0
    for value in obj_schema.values():
        if isinstance(value, dict):
            max_depth = max(max_depth, 1 + get_dependency_depth(value))
        elif isinstance(value, list) and len(value) > 0 and isinstance(value[0], dict):
            max_depth = max(max_depth, 1 + get_dependency_depth(value[0]))
    return max_depth

def generate_enum_mappings(operation: str) -> str:
    mappings = f"""
static const enum_mapping<tune_params::aarch64_autoprefetch_model>
  autoprefetcher_model_mappings[] = {{
#define AARCH64_AUTOPREFETCH_MODE(NAME, ENUM_VALUE) {{NAME, tune_params::ENUM_VALUE}},
#include "aarch64-tuning-enums.def"
}};

static const enum_mapping<aarch64_ldp_stp_policy> ldp_policy_model_mappings[] = {{
#define AARCH64_LDP_STP_POLICY(NAME, ENUM_VALUE) {{NAME, ENUM_VALUE}},
#include "aarch64-tuning-enums.def"
}};

static const enum_mapping<aarch64_ldp_stp_policy> stp_policy_model_mappings[] = {{
#define AARCH64_LDP_STP_POLICY(NAME, ENUM_VALUE) {{NAME, ENUM_VALUE}},
#include "aarch64-tuning-enums.def"
}};
"""
    return mappings

def generate_all_functions(schema_file: str, operation: str) -> str:
    schema_str = extract_schema_from_header(schema_file)
    schema = json.loads(schema_str)
    tune_params_schema = schema.get("tune_params", {})

    all_objects_with_paths = collect_all_objects_with_paths(tune_params_schema)

    function_map = {}
    for path_key, (path, obj_schema) in all_objects_with_paths.items():
        if path:
            full_name = "_".join(path)
            function_map[path_key] = f"{operation}_{full_name}"
        else:
            function_map[path_key] = f"{operation}_{path_key}"

    """ Structures can have nested structures that may not have been defined yet.
    Therefore, we need to sort the objects by dependency depth and define
    functions for the inner structures first."""
    sorted_objects = sorted(
        all_objects_with_paths.items(), key=lambda x: get_dependency_depth(x[1][1])
    )

    generated_functions = []
    generated_functions.append(generate_enum_mappings(operation))

    for path_key, (path, obj_schema) in sorted_objects:
        # Use the full path for function generation
        if path:
            full_name = "_".join(path)
            local_name = path[-1]
        else:
            full_name = path_key
            local_name = path_key

        function_str = generate_function(
            operation, full_name, local_name, obj_schema, path, function_map
        )
        generated_functions.append("\n".join(function_str))

    main_function = generate_function(
        operation, "tunings", "tunings", tune_params_schema, [], function_map
    )
    generated_functions.append("\n".join(main_function))
    return "\n\n".join(generated_functions)

def write_generated_include_file(
    output_file_path: str, generated_code: str, operation: str
) -> None:
    header_comment = f"""/* This file is auto-generated by aarch64-generate-json-tuning-routines.py.  */
/* Copyright The GNU Toolchain Authors.
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

/* This file contains the auto-generated {operation} functions for JSON tuning parameters.  */

"""

    try:
        with open(output_file_path, "w") as f:
            f.write(header_comment)
            f.write(generated_code)
        print(f"Successfully generated {output_file_path}")
    except Exception as e:
        print(f"Error writing to {output_file_path}: {e}")

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--generate-only',
                        choices=['parser', 'printer'],
                        help='Generate only parser or printer file. If not specified, generates both.')
    args = parser.parse_args()

    try:
        script_dir = os.path.dirname(os.path.abspath(__file__))

        schema_file = os.path.join(script_dir, "aarch64-json-schema.h")
        parser_inc_file = os.path.join(
            script_dir, "aarch64-json-tunings-parser-generated.inc"
        )
        printer_inc_file = os.path.join(
            script_dir, "aarch64-json-tunings-printer-generated.inc"
        )
        if args.generate_only is None or args.generate_only == 'parser':
            parser_generated_code = generate_all_functions(schema_file, "parse")
            write_generated_include_file(parser_inc_file, parser_generated_code, "parser")

        if args.generate_only is None or args.generate_only == 'printer':
            serializer_generated_code = generate_all_functions(schema_file, "serialize")
            write_generated_include_file(
                printer_inc_file, serializer_generated_code, "serializer"
            )

        print(f"Generated files in: {script_dir}")

    except Exception as e:
        print(f"Error: {e}")
        return 1

    return 0

if __name__ == "__main__":
    exit(main())
