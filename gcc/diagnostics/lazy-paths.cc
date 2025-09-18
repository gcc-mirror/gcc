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
#include "diagnostics/lazy-paths.h"
#include "selftest.h"
#include "diagnostics/selftest-context.h"
#include "diagnostics/selftest-paths.h"
#include "diagnostics/text-sink.h"

using namespace diagnostics::paths;

/* class lazy_path : public path.  */

/* Implementation of path vfuncs in terms of a lazily-generated
   path.  */

unsigned
lazy_path::num_events () const
{
  lazily_generate_path ();
  return m_inner_path->num_events ();
}

const event &
lazy_path::get_event (int idx) const
{
  lazily_generate_path ();
  return m_inner_path->get_event (idx);
}

unsigned
lazy_path::num_threads () const
{
  lazily_generate_path ();
  return m_inner_path->num_threads ();
}

const thread &
lazy_path::get_thread (thread_id_t idx) const
{
  lazily_generate_path ();
  return m_inner_path->get_thread (idx);
}

bool
lazy_path::same_function_p (int event_idx_a,
			    int event_idx_b) const
{
  lazily_generate_path ();
  return m_inner_path->same_function_p (event_idx_a, event_idx_b);
}

void
lazy_path::lazily_generate_path () const
{
  if (!m_inner_path)
    m_inner_path = make_inner_path ();
  gcc_assert (m_inner_path != nullptr);
}

#if CHECKING_P

namespace diagnostics {
namespace selftest {

using auto_fix_quotes = ::selftest::auto_fix_quotes;

class test_lazy_path : public lazy_path
{
public:
  test_lazy_path (pretty_printer &pp)
  : lazy_path (m_logical_loc_mgr),
    m_pp (pp)
  {
  }
  std::unique_ptr<path> make_inner_path () const final override
  {
    auto path
      = std::make_unique<paths::selftest::test_path> (m_logical_loc_mgr,
						      &m_pp);
    path->add_event (UNKNOWN_LOCATION, "foo", 0, "first %qs", "free");
    path->add_event (UNKNOWN_LOCATION, "foo", 0, "double %qs", "free");
    return path;
  }
private:
  mutable logical_locations::selftest::test_manager m_logical_loc_mgr;
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

/* Implementation of diagnostics::option_id_manager for which all
   options are disabled, for use in selftests.
   Note that this is *not* called for option_id (0), which
   means "always warn"  */

class all_warnings_disabled : public diagnostics::option_id_manager
{
public:
  int option_enabled_p (diagnostics::option_id) const final override
  {
    /* Treat all options as disabled.  */
    return 0;
  }
  char *make_option_name (diagnostics::option_id,
			  enum kind,
			  enum kind) const final override
  {
    return nullptr;
  }
  char *make_option_url (diagnostics::option_id) const final override
  {
    return nullptr;
  }
};

static void
test_emission (pretty_printer *event_pp)
{
  struct test_rich_location : public rich_location
  {
    test_rich_location (pretty_printer &event_pp)
    : rich_location (line_table, UNKNOWN_LOCATION),
      m_path (event_pp)
    {
      set_path (&m_path);
    }
    test_lazy_path m_path;
  };

  /* Verify that we don't bother generating the inner path if the warning
     is skipped.  */
  {
    test_context dc;
    dc.set_option_id_manager (std::make_unique<all_warnings_disabled> (), 0);

    test_rich_location rich_loc (*event_pp);
    ASSERT_FALSE (rich_loc.m_path.generated_p ());

    diagnostics::option_id opt_id (42); // has to be non-zero
    bool emitted
      = dc.emit_diagnostic_with_group (kind::warning, rich_loc, nullptr,
				       opt_id,
				       "this warning should be skipped");
    ASSERT_FALSE (emitted);
    ASSERT_FALSE (rich_loc.m_path.generated_p ());
  }

  /* Verify that we *do* generate the inner path for a diagnostic that
     is emitted, such as an error.  */
  {
    test_context dc;

    test_rich_location rich_loc (*event_pp);
    ASSERT_FALSE (rich_loc.m_path.generated_p ());

    bool emitted
      = dc.emit_diagnostic_with_group (kind::error, rich_loc, nullptr, 0,
				       "this is a test");
    ASSERT_TRUE (emitted);
    ASSERT_TRUE (rich_loc.m_path.generated_p ());

    /* Verify that the path works as expected.  */
    dc.set_path_format (DPF_INLINE_EVENTS);
    diagnostics::text_sink sink_ (dc);
    pp_buffer (sink_.get_printer ())->m_flush_p = false;
    sink_.print_path (rich_loc.m_path);
    ASSERT_STREQ (pp_formatted_text (sink_.get_printer ()),
		  "  `foo': event 1\n"
		  " (1): first `free'\n"
		  "  `foo': event 2\n"
		  " (2): double `free'\n");
  }
}

/* Run all of the selftests within this file.  */

void
lazy_paths_cc_tests ()
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

} // namespace diagnostics::selftest
} // namespace diagnostics

#endif /* #if CHECKING_P */
