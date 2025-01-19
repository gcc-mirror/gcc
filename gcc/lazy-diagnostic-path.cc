/* Helper class for deferring path creation until a diagnostic is emitted.
   Copyright (C) 2019-2025 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>

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
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "version.h"
#include "intl.h"
#include "diagnostic.h"
#include "lazy-diagnostic-path.h"
#include "make-unique.h"
#include "selftest.h"
#include "selftest-diagnostic.h"
#include "simple-diagnostic-path.h"
#include "gcc-rich-location.h"
#include "diagnostic-format-text.h"

/* class lazy_diagnostic_path : public diagnostic_path.  */

/* Implementation of diagnostic_path vfuncs in terms of a lazily-generated
   path.  */

unsigned
lazy_diagnostic_path::num_events () const
{
  lazily_generate_path ();
  return m_inner_path->num_events ();
}

const diagnostic_event &
lazy_diagnostic_path::get_event (int idx) const
{
  lazily_generate_path ();
  return m_inner_path->get_event (idx);
}

unsigned
lazy_diagnostic_path::num_threads () const
{
  lazily_generate_path ();
  return m_inner_path->num_threads ();
}

const diagnostic_thread &
lazy_diagnostic_path::get_thread (diagnostic_thread_id_t idx) const
{
  lazily_generate_path ();
  return m_inner_path->get_thread (idx);
}

bool
lazy_diagnostic_path::same_function_p (int event_idx_a,
					   int event_idx_b) const
{
  lazily_generate_path ();
  return m_inner_path->same_function_p (event_idx_a, event_idx_b);
}

void
lazy_diagnostic_path::lazily_generate_path () const
{
  if (!m_inner_path)
    m_inner_path = make_inner_path ();
  gcc_assert (m_inner_path != nullptr);
}

#if CHECKING_P

namespace selftest {

class test_lazy_path : public lazy_diagnostic_path
{
public:
  test_lazy_path (pretty_printer &pp)
  : m_pp (pp)
  {
  }
  std::unique_ptr<diagnostic_path> make_inner_path () const final override
  {
    tree fntype_void_void
      = build_function_type_array (void_type_node, 0, NULL);
    tree fndecl_foo = build_fn_decl ("foo", fntype_void_void);
    auto path = ::make_unique<simple_diagnostic_path> (&m_pp);
    path->add_event (UNKNOWN_LOCATION, fndecl_foo, 0, "first %qs", "free");
    path->add_event (UNKNOWN_LOCATION, fndecl_foo, 0, "double %qs", "free");
    return path;
  }
private:
  pretty_printer &m_pp;
};

static void
test_intraprocedural_path (pretty_printer *event_pp)
{
  test_lazy_path path (*event_pp);
  ASSERT_FALSE (path.generated_p ());
  ASSERT_EQ (path.num_events (), 2);
  ASSERT_TRUE (path.generated_p ());
  ASSERT_EQ (path.num_threads (), 1);
  ASSERT_FALSE (path.interprocedural_p ());
  ASSERT_STREQ (path.get_event (0).get_desc (*event_pp).get (),
		"first `free'");
  ASSERT_STREQ (path.get_event (1).get_desc (*event_pp).get (),
		"double `free'");
}

/* Implementation of diagnostic_option_manager for which all
   options are disabled, for use in selftests.
   Note that this is *not* called for diagnostic_option_id (0), which
   means "always warn"  */

class all_warnings_disabled : public diagnostic_option_manager
{
public:
  int option_enabled_p (diagnostic_option_id) const final override
  {
    /* Treat all options as disabled.  */
    return 0;
  }
  char *make_option_name (diagnostic_option_id,
			  diagnostic_t,
			  diagnostic_t) const final override
  {
    return nullptr;
  }
  char *make_option_url (diagnostic_option_id) const final override
  {
    return nullptr;
  }
};

static void
test_emission (pretty_printer *event_pp)
{
  struct test_rich_location : public gcc_rich_location
  {
    test_rich_location (pretty_printer &event_pp)
    : gcc_rich_location (UNKNOWN_LOCATION),
      m_path (event_pp)
    {
      set_path (&m_path);
    }
    test_lazy_path m_path;
  };

  /* Verify that we don't bother generating the inner path if the warning
     is skipped.  */
  {
    test_diagnostic_context dc;
    dc.set_option_manager (::make_unique<all_warnings_disabled> (), 0);

    test_rich_location rich_loc (*event_pp);
    ASSERT_FALSE (rich_loc.m_path.generated_p ());

    diagnostic_option_id option_id (42); // has to be non-zero
    bool emitted
      = dc.emit_diagnostic_with_group (DK_WARNING, rich_loc, nullptr,
				       option_id,
				       "this warning should be skipped");
    ASSERT_FALSE (emitted);
    ASSERT_FALSE (rich_loc.m_path.generated_p ());
  }

  /* Verify that we *do* generate the inner path for a diagnostic that
     is emitted, such as an error.  */
  {
    test_diagnostic_context dc;

    test_rich_location rich_loc (*event_pp);
    ASSERT_FALSE (rich_loc.m_path.generated_p ());

    bool emitted
      = dc.emit_diagnostic_with_group (DK_ERROR, rich_loc, nullptr, 0,
				       "this is a test");
    ASSERT_TRUE (emitted);
    ASSERT_TRUE (rich_loc.m_path.generated_p ());

    /* Verify that the path works as expected.  */
    dc.set_path_format (DPF_INLINE_EVENTS);
    diagnostic_text_output_format sink (dc);
    pp_buffer (sink.get_printer ())->m_flush_p = false;
    sink.print_path (rich_loc.m_path);
    ASSERT_STREQ (pp_formatted_text (sink.get_printer ()),
		  "  `foo': event 1\n"
		  " (1): first `free'\n"
		  "  `foo': event 2\n"
		  " (2): double `free'\n");
  }
}

/* Run all of the selftests within this file.  */

void
lazy_diagnostic_path_cc_tests ()
{
  /* In a few places we use the global dc's printer to determine
     colorization so ensure this off during the tests.  */
  pretty_printer *global_pp = global_dc->get_reference_printer ();
  const bool saved_show_color = pp_show_color (global_pp);
  pp_show_color (global_pp) = false;

  auto_fix_quotes fix_quotes;
  std::unique_ptr<pretty_printer> event_pp
    = std::unique_ptr<pretty_printer> (global_pp->clone ());

  test_intraprocedural_path (event_pp.get ());
  test_emission (event_pp.get ());

  pp_show_color (global_pp) = saved_show_color;
}

} // namespace selftest

#endif /* #if CHECKING_P */
