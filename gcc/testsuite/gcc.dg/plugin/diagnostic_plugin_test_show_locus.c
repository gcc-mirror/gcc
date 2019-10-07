/* { dg-options "-O" } */

/* This plugin exercises the diagnostics-printing code.

   The goal is to unit-test the range-printing code without needing any
   correct range data within the compiler's IR.  We can't use any real
   diagnostics for this, so we have to fake it, hence this plugin.

   There are two test files used with this code:

     diagnostic-test-show-locus-ascii-bw.c
     ..........................-ascii-color.c

   to exercise uncolored vs colored output by supplying plugin arguments
   to hack in the desired behavior:

     -fplugin-arg-diagnostic_plugin_test_show_locus-color

   The test files contain functions, but the body of each
   function is disabled using the preprocessor.  The plugin detects
   the functions by name, and inject diagnostics within them, using
   hard-coded locations relative to the top of each function.

   The plugin uses a function "get_loc" below to map from line/column
   numbers to location_t, and this relies on input_location being in
   the same ordinary line_map as the locations in question.  The plugin
   runs after parsing, so input_location will be at the end of the file.

   This need for all of the test code to be in a single ordinary line map
   means that each test file needs to have a very long line near the top
   (potentially to cover the extra byte-count of colorized data),
   to ensure that further very long lines don't start a new linemap.
   This also means that we can't use macros in the test files.  */

#include "gcc-plugin.h"
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "stringpool.h"
#include "toplev.h"
#include "basic-block.h"
#include "hash-table.h"
#include "vec.h"
#include "ggc.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "tree.h"
#include "tree-pass.h"
#include "intl.h"
#include "plugin-version.h"
#include "diagnostic.h"
#include "context.h"
#include "print-tree.h"
#include "gcc-rich-location.h"

int plugin_is_GPL_compatible;

const pass_data pass_data_test_show_locus =
{
  GIMPLE_PASS, /* type */
  "test_show_locus", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_test_show_locus : public gimple_opt_pass
{
public:
  pass_test_show_locus(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_test_show_locus, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) { return true; }
  virtual unsigned int execute (function *);

}; // class pass_test_show_locus

/* Given LINE_NUM and COL_NUM, generate a location_t in the
   current file, relative to input_location.  This relies on the
   location being expressible in the same ordinary line_map as
   input_location (which is typically at the end of the source file
   when this is called).  Hence the test files we compile with this
   plugin must have an initial very long line (to avoid long lines
   starting a new line map), and must not use macros.

   COL_NUM uses the Emacs convention of 0-based column numbers.  */

static location_t
get_loc (unsigned int line_num, unsigned int col_num)
{
  /* Use input_location to get the relevant line_map */
  const struct line_map_ordinary *line_map
    = (const line_map_ordinary *)(linemap_lookup (line_table,
						  input_location));

  /* Convert from 0-based column numbers to 1-based column numbers.  */
  location_t loc
    = linemap_position_for_line_and_column (line_table,
					    line_map,
					    line_num, col_num + 1);

  return loc;
}

/* Was "color" passed in as a plugin argument?  */
static bool force_show_locus_color = false;

/* We want to verify the colorized output of diagnostic_show_locus,
   but turning on colorization for everything confuses "dg-warning" etc.
   Hence we special-case it within this plugin by using this modified
   version of default_diagnostic_finalizer, which, if "color" is
   passed in as a plugin argument turns on colorization, but just
   for diagnostic_show_locus.  */

static void
custom_diagnostic_finalizer (diagnostic_context *context,
			     diagnostic_info *diagnostic,
			     diagnostic_t)
{
  bool old_show_color = pp_show_color (context->printer);
  if (force_show_locus_color)
    pp_show_color (context->printer) = true;
  char *saved_prefix = pp_take_prefix (context->printer);
  pp_set_prefix (context->printer, NULL);
  diagnostic_show_locus (context, diagnostic->richloc, diagnostic->kind);
  pp_show_color (context->printer) = old_show_color;
  pp_set_prefix (context->printer, saved_prefix);
  pp_flush (context->printer);
}

/* Add a location to RICHLOC with caret==start at START, ranging to FINISH.  */

static void
add_range (rich_location *richloc, location_t start, location_t finish,
	   enum range_display_kind range_display_kind
	     = SHOW_RANGE_WITHOUT_CARET,
	   const range_label *label = NULL)
{
  richloc->add_range (make_location (start, start, finish), range_display_kind,
		      label);
}

/* Exercise the diagnostic machinery to emit various warnings,
   for use by diagnostic-test-show-locus-*.c.

   We inject each warning relative to the start of a function,
   which avoids lots of hardcoded absolute locations.  */

static void
test_show_locus (function *fun)
{
  tree fndecl = fun->decl;
  tree identifier = DECL_NAME (fndecl);
  const char *fnname = IDENTIFIER_POINTER (identifier);
  location_t fnstart = fun->function_start_locus;
  int fnstart_line = LOCATION_LINE (fnstart);

  diagnostic_finalizer (global_dc) = custom_diagnostic_finalizer;

  /* Hardcode the "terminal width", to verify the behavior of
     very wide lines.  */
  global_dc->caret_max_width = 70;

  if (0 == strcmp (fnname, "test_simple"))
    {
      const int line = fnstart_line + 2;
      rich_location richloc (line_table, get_loc (line, 15));
      add_range (&richloc, get_loc (line, 10), get_loc (line, 14));
      add_range (&richloc, get_loc (line, 16), get_loc (line, 16));
      warning_at (&richloc, 0, "test");
    }

  if (0 == strcmp (fnname, "test_simple_2"))
    {
      const int line = fnstart_line + 2;
      rich_location richloc (line_table, get_loc (line, 24));
      add_range (&richloc, get_loc (line, 6), get_loc (line, 22));
      add_range (&richloc, get_loc (line, 26), get_loc (line, 43));
      warning_at (&richloc, 0, "test");
    }

  if (0 == strcmp (fnname, "test_multiline"))
    {
      const int line = fnstart_line + 2;
      text_range_label label ("label");
      rich_location richloc (line_table, get_loc (line + 1, 7), &label);
      add_range (&richloc, get_loc (line, 7), get_loc (line, 23));
      add_range (&richloc, get_loc (line + 1, 9), get_loc (line + 1, 26));
      warning_at (&richloc, 0, "test");
    }

  if (0 == strcmp (fnname, "test_many_lines"))
    {
      const int line = fnstart_line + 2;
      text_range_label label0 ("label 0");
      text_range_label label1 ("label 1");
      text_range_label label2 ("label 2");
      rich_location richloc (line_table, get_loc (line + 5, 7), &label0);
      add_range (&richloc, get_loc (line, 7), get_loc (line + 4, 65),
		 SHOW_RANGE_WITHOUT_CARET, &label1);
      add_range (&richloc, get_loc (line + 5, 9), get_loc (line + 10, 61),
		 SHOW_RANGE_WITHOUT_CARET, &label2);
      warning_at (&richloc, 0, "test");
    }

  /* Example of a rich_location where the range is larger than
     one character.  */
  if (0 == strcmp (fnname, "test_richloc_from_proper_range"))
    {
      const int line = fnstart_line + 2;
      location_t start = get_loc (line, 12);
      location_t finish = get_loc (line, 16);
      rich_location richloc (line_table, make_location (start, start, finish));
      warning_at (&richloc, 0, "test");
    }

  /* Example of a single-range location where the range starts
     before the caret.  */
  if (0 == strcmp (fnname, "test_caret_within_proper_range"))
    {
      const int line = fnstart_line + 2;
      warning_at (make_location (get_loc (line, 16), get_loc (line, 12),
				 get_loc (line, 20)),
		  0, "test");
    }

  /* Example of a very wide line, where the information of interest
     is beyond the width of the terminal (hardcoded above), with
     a secondary location that exactly fits on the left-margin.  */
  if (0 == strcmp (fnname, "test_very_wide_line"))
    {
      const int line = fnstart_line + 2;
      global_dc->show_ruler_p = true;
      text_range_label label0 ("label 0");
      text_range_label label1 ("label 1");
      rich_location richloc (line_table,
			     make_location (get_loc (line, 94),
					    get_loc (line, 90),
					    get_loc (line, 98)),
			     &label0);
      richloc.add_range (get_loc (line, 35), SHOW_RANGE_WITHOUT_CARET,
			 &label1);
      richloc.add_fixit_replace ("bar * foo");
      warning_at (&richloc, 0, "test");
      global_dc->show_ruler_p = false;
    }

  /* Likewise, but with a secondary location that's immediately before
     the left margin; the location and label should be gracefully dropped.  */
  if (0 == strcmp (fnname, "test_very_wide_line_2"))
    {
      const int line = fnstart_line + 2;
      global_dc->show_ruler_p = true;
      text_range_label label0 ("label 0");
      text_range_label label1 ("label 1");
      rich_location richloc (line_table,
			     make_location (get_loc (line, 94),
					    get_loc (line, 90),
					    get_loc (line, 98)),
			     &label0);
      richloc.add_fixit_replace ("bar * foo");
      richloc.add_range (get_loc (line, 34), SHOW_RANGE_WITHOUT_CARET,
			 &label1);
      warning_at (&richloc, 0, "test");
      global_dc->show_ruler_p = false;
    }

  /* Example of multiple carets.  */
  if (0 == strcmp (fnname, "test_multiple_carets"))
    {
      const int line = fnstart_line + 2;
      location_t caret_a = get_loc (line, 7);
      location_t caret_b = get_loc (line, 11);
      rich_location richloc (line_table, caret_a);
      add_range (&richloc, caret_b, caret_b, SHOW_RANGE_WITH_CARET);
      global_dc->caret_chars[0] = 'A';
      global_dc->caret_chars[1] = 'B';
      warning_at (&richloc, 0, "test");
      global_dc->caret_chars[0] = '^';
      global_dc->caret_chars[1] = '^';
    }

  /* Tests of rendering fixit hints.  */
  if (0 == strcmp (fnname, "test_fixit_insert"))
    {
      const int line = fnstart_line + 2;
      location_t start = get_loc (line, 19);
      location_t finish = get_loc (line, 22);
      rich_location richloc (line_table, make_location (start, start, finish));
      richloc.add_fixit_insert_before ("{");
      richloc.add_fixit_insert_after ("}");
      warning_at (&richloc, 0, "example of insertion hints");
    }

  if (0 == strcmp (fnname, "test_fixit_insert_newline"))
    {
      const int line = fnstart_line + 6;
      location_t line_start = get_loc (line, 0);
      location_t case_start = get_loc (line, 4);
      location_t case_finish = get_loc (line, 11);
      location_t case_loc = make_location (case_start, case_start, case_finish);
      rich_location richloc (line_table, case_loc);
      richloc.add_fixit_insert_before (line_start, "      break;\n");
      warning_at (&richloc, 0, "example of newline insertion hint");
    }

  if (0 == strcmp (fnname, "test_fixit_remove"))
    {
      const int line = fnstart_line + 2;
      location_t start = get_loc (line, 8);
      location_t finish = get_loc (line, 8);
      rich_location richloc (line_table, make_location (start, start, finish));
      source_range src_range;
      src_range.m_start = start;
      src_range.m_finish = finish;
      richloc.add_fixit_remove (src_range);
      warning_at (&richloc, 0, "example of a removal hint");
    }

  if (0 == strcmp (fnname, "test_fixit_replace"))
    {
      const int line = fnstart_line + 2;
      location_t start = get_loc (line, 2);
      location_t finish = get_loc (line, 19);
      rich_location richloc (line_table, make_location (start, start, finish));
      source_range src_range;
      src_range.m_start = start;
      src_range.m_finish = finish;
      richloc.add_fixit_replace (src_range, "gtk_widget_show_all");
      warning_at (&richloc, 0, "example of a replacement hint");
    }

  if (0 == strcmp (fnname, "test_mutually_exclusive_suggestions"))
    {
      const int line = fnstart_line + 2;
      location_t start = get_loc (line, 2);
      location_t finish = get_loc (line, 9);
      source_range src_range;
      src_range.m_start = start;
      src_range.m_finish = finish;

      {
	rich_location richloc (line_table, make_location (start, start, finish));
	richloc.add_fixit_replace (src_range, "replacement_1");
	richloc.fixits_cannot_be_auto_applied ();
	warning_at (&richloc, 0, "warning 1");
      }

      {
	rich_location richloc (line_table, make_location (start, start, finish));
	richloc.add_fixit_replace (src_range, "replacement_2");
	richloc.fixits_cannot_be_auto_applied ();
	warning_at (&richloc, 0, "warning 2");
      }
    }  

  /* Tests of gcc_rich_location::add_fixit_insert_formatted.  */

  if (0 == strcmp (fnname, "test_add_fixit_insert_formatted_single_line"))
    {
      const int line = fnstart_line + 1;
      location_t insertion_point = get_loc (line, 3);
      location_t indent = get_loc (line, 2);
      gcc_rich_location richloc (insertion_point);
      richloc.add_fixit_insert_formatted ("INSERTED-CONTENT",
					  insertion_point, indent);
      inform (&richloc, "single-line insertion");
    }

  if (0 == strcmp (fnname, "test_add_fixit_insert_formatted_multiline"))
    {
      location_t insertion_point = fun->function_end_locus;
      location_t indent = get_loc (fnstart_line + 1, 2);
      gcc_rich_location richloc (insertion_point);
      richloc.add_fixit_insert_formatted ("INSERTED-CONTENT",
					  insertion_point, indent);
      inform (&richloc, "multiline insertion");
    }

  /* Example of two carets where both carets appear to have an off-by-one
     error appearing one column early.
     Seen with gfortran.dg/associate_5.f03.
     In an earlier version of the printer, the printing of caret 0 aka
     "1" was suppressed due to it appearing within the leading whitespace
     before the text in its line.  Ensure that we at least faithfully
     print both carets, at the given (erroneous) locations.  */
  if (0 == strcmp (fnname, "test_caret_on_leading_whitespace"))
    {
      const int line = fnstart_line + 3;
      location_t caret_a = get_loc (line, 5);
      location_t caret_b = get_loc (line - 1, 19);
      rich_location richloc (line_table, caret_a);
      richloc.add_range (caret_b, SHOW_RANGE_WITH_CARET);
      global_dc->caret_chars[0] = '1';
      global_dc->caret_chars[1] = '2';
      warning_at (&richloc, 0, "test");
      global_dc->caret_chars[0] = '^';
      global_dc->caret_chars[1] = '^';
    }

  /* Example of using the "%q+D" format code, which as well as printing
     a quoted decl, overrides the given location to use the location of
     the decl.  */
  if (0 == strcmp (fnname, "test_percent_q_plus_d"))
    {
      const int line = fnstart_line + 3;
      tree local = (*fun->local_decls)[0];
      warning_at (input_location, 0,
		  "example of plus in format code for %q+D", local);
    }

  /* Example of many locations and many fixits.
     Underline (separately) every word in a comment, and convert them
     to upper case.  Give all of the ranges labels (sharing one label).  */
  if (0 == strcmp (fnname, "test_many_nested_locations"))
    {
      const char *file = LOCATION_FILE (fnstart);
      const int start_line = fnstart_line + 2;
      const int finish_line = start_line + 7;
      location_t loc = get_loc (start_line - 1, 2);
      text_range_label label ("label");
      rich_location richloc (line_table, loc);
      for (int line = start_line; line <= finish_line; line++)
	{
	  char_span content = location_get_source_line (file, line);
	  gcc_assert (content);
	  /* Split line up into words.  */
	  for (int idx = 0; idx < content.length (); idx++)
	    {
	      if (ISALPHA (content[idx]))
		{
		  int start_idx = idx;
		  while (idx < content.length () && ISALPHA (content[idx]))
		    idx++;
		  if (idx == content.length () || !ISALPHA (content[idx]))
		    {
		      location_t start_of_word = get_loc (line, start_idx);
		      location_t end_of_word = get_loc (line, idx - 1);
		      location_t word
			= make_location (start_of_word, start_of_word,
					 end_of_word);
		      richloc.add_range (word, SHOW_RANGE_WITH_CARET, &label);

		      /* Add a fixit, converting to upper case.  */
		      char_span word_span = content.subspan (start_idx, idx - start_idx);
		      char *copy = word_span.xstrdup ();
		      for (char *ch = copy; *ch; ch++)
			*ch = TOUPPER (*ch);
		      richloc.add_fixit_replace (word, copy);
		      free (copy);
		    }
		}
	    }
	}
      /* Verify that we added enough locations to fully exercise
	 rich_location.  We want to exceed both the
	 statically-allocated buffer in class rich_location,
	 and then trigger a reallocation of the dynamic buffer.  */
      gcc_assert (richloc.get_num_locations () > 3 + (2 * 16));
      warning_at (&richloc, 0, "test of %i locations",
		  richloc.get_num_locations ());
    }
}

unsigned int
pass_test_show_locus::execute (function *fun)
{
  test_show_locus (fun);
  return 0;
}

static gimple_opt_pass *
make_pass_test_show_locus (gcc::context *ctxt)
{
  return new pass_test_show_locus (ctxt);
}

int
plugin_init (struct plugin_name_args *plugin_info,
	     struct plugin_gcc_version *version)
{
  struct register_pass_info pass_info;
  const char *plugin_name = plugin_info->base_name;
  int argc = plugin_info->argc;
  struct plugin_argument *argv = plugin_info->argv;

  if (!plugin_default_version_check (version, &gcc_version))
    return 1;

  for (int i = 0; i < argc; i++)
    {
      if (0 == strcmp (argv[i].key, "color"))
	force_show_locus_color = true;
    }

  pass_info.pass = make_pass_test_show_locus (g);
  pass_info.reference_pass_name = "ssa";
  pass_info.ref_pass_instance_number = 1;
  pass_info.pos_op = PASS_POS_INSERT_AFTER;
  register_callback (plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL,
		     &pass_info);

  return 0;
}
