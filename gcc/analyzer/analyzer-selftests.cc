/* Selftest support for the analyzer.
   Copyright (C) 2019-2026 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

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

#include "analyzer/common.h"

#include "stringpool.h"
#include "stor-layout.h"

#include "analyzer/analyzer-selftests.h"

#if CHECKING_P

namespace ana {

namespace selftest {

/* Build a VAR_DECL named NAME of type TYPE, simulating a file-level
   static variable.  */

tree
build_global_decl (const char *name, tree type)
{
  tree decl = build_decl (UNKNOWN_LOCATION, VAR_DECL,
			  get_identifier (name), type);
  TREE_STATIC (decl) = 1;
  return decl;
}

tree
make_string_constant (const char *str)
{
  tree string_literal = build_string_literal (strlen (str) + 1, str);
  gcc_assert (TREE_CODE (string_literal) == ADDR_EXPR);
  tree array_ref = TREE_OPERAND (string_literal, 0);
  gcc_assert (TREE_CODE (array_ref) == ARRAY_REF);
  tree string_cst = TREE_OPERAND (array_ref, 0);
  gcc_assert (TREE_CODE (string_cst) == STRING_CST);
  return string_cst;
}

/* Helper function for selftests.  Create a struct or union type named NAME,
   with the fields given by the FIELD_DECLS in FIELDS.
   If IS_STRUCT is true create a RECORD_TYPE (aka a struct), otherwise
   create a UNION_TYPE.  */

tree
make_test_compound_type (const char *name, bool is_struct,
			 const std::vector<tree> fields)
{
  tree t = make_node (is_struct ? RECORD_TYPE : UNION_TYPE);
  TYPE_NAME (t) = get_identifier (name);
  TYPE_SIZE (t) = 0;

  tree fieldlist = NULL_TREE;
  for (auto field : fields)
    {
      gcc_assert (TREE_CODE (field) == FIELD_DECL);
      DECL_CONTEXT (field) = t;
      fieldlist = chainon (field, fieldlist);
    }
  fieldlist = nreverse (fieldlist);
  TYPE_FIELDS (t) = fieldlist;

  layout_type (t);
  return t;
}

/* Selftest fixture for creating the type "struct coord {int x; int y; };".  */

coord_test::coord_test ()
{
  std::vector<tree> fields;
  m_x_field = build_decl (UNKNOWN_LOCATION, FIELD_DECL,
			  get_identifier ("x"), integer_type_node);
  fields.push_back (m_x_field);
  m_y_field = build_decl (UNKNOWN_LOCATION, FIELD_DECL,
			  get_identifier ("y"), integer_type_node);
  fields.push_back (m_y_field);
  m_coord_type = make_test_compound_type ("coord", true, std::move (fields));
}

/* Run all analyzer-specific selftests.  */

void
run_analyzer_selftests ()
{
#if ENABLE_ANALYZER
  analyzer_access_diagram_cc_tests ();
  analyzer_complexity_cc_tests ();
  analyzer_concrete_ranges_cc_tests ();
  analyzer_constraint_manager_cc_tests ();
  analyzer_function_set_cc_tests ();
  analyzer_integer_cc_tests ();
  analyzer_integer_with_unit_cc_tests ();
  analyzer_program_point_cc_tests ();
  analyzer_program_state_cc_tests ();
  analyzer_region_model_manager_cc_tests ();
  analyzer_region_model_cc_tests ();
  analyzer_sm_file_cc_tests ();
  analyzer_sm_signal_cc_tests ();
  analyzer_store_cc_tests ();
  analyzer_string_scan_cc_tests ();
  analyzer_symbolic_bit_contents_cc_tests ();
  analyzer_symbolic_bit_vectors_cc_tests ();
  analyzer_symbolic_memory_ranges_cc_tests ();
  analyzer_symbolic_nodes_cc_tests ();
  analyzer_symbolic_pointers_cc_tests ();
  analyzer_symbolic_ranges_cc_tests ();
  analyzer_symbolic_scalars_cc_tests ();
#endif /* #if ENABLE_ANALYZER */
}

} /* end of namespace selftest.  */

} // namespace ana

#endif /* #if CHECKING_P */
