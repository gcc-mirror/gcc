/* Plugin for testing how gracefully we degrade in the face of very
   large source files.  */

#include "config.h"
#include "gcc-plugin.h"
#include "system.h"
#include "coretypes.h"
#include "spellcheck.h"
#include "diagnostic.h"
#include "diagnostic-format-text.h"

int plugin_is_GPL_compatible;

static location_t base_location;

/* Callback handler for the PLUGIN_PRAGMAS event; pretend we parsed a
   very large include file.  This is used to set the initial line table
   offset for the preprocessor, to make it appear as if we had parsed a
   very large file.  PRAGMA_START_UNIT is not suitable here as is not
   invoked during the preprocessor stage.  */

static void
on_pragma_registration (void */*gcc_data*/, void */*user_data*/)
{
  /* Act as if we've already parsed a large body of code;
     so that we can simulate various fallbacks in libcpp:

     0x50000001 > LINE_MAP_MAX_LOCATION_WITH_PACKED_RANGES:
     this will trigger the creation of line maps with range_bits == 0
     so that all ranges will be stored in the ad-hoc lookaside.

     0x60000001 > LINE_MAP_MAX_LOCATION_WITH_COLS:
     this will trigger the creation of line maps with column_bits == 0
     and hence we will immediately degrade to having locations in which
     column number is 0. */
  line_table->highest_location = base_location;
}

/* We add some extra testing during diagnostics by chaining up
   to the text finalizer.  */

static diagnostic_text_finalizer_fn original_text_finalizer = NULL;

static void
verify_unpacked_ranges  (diagnostic_text_output_format &text_output,
			 const diagnostic_info *diagnostic,
			 diagnostic_t orig_diag_kind)
{
  /* Verify that the locations are ad-hoc, not packed. */
  location_t loc = diagnostic_location (diagnostic);
  gcc_assert (IS_ADHOC_LOC (loc));

  /* We're done testing; chain up to original text finalizer.  */
  gcc_assert (original_text_finalizer);
  original_text_finalizer (text_output, diagnostic, orig_diag_kind);
}

static void
verify_no_columns  (diagnostic_text_output_format &text_output,
		    const diagnostic_info *diagnostic,
		    diagnostic_t orig_diag_kind)
{
  /* Verify that the locations have no columns. */
  location_t loc = diagnostic_location (diagnostic);
  gcc_assert (LOCATION_COLUMN (loc) == 0);

  /* We're done testing; chain up to original text finalizer.  */
  gcc_assert (original_text_finalizer);
  original_text_finalizer (text_output, diagnostic, orig_diag_kind);
}

int
plugin_init (struct plugin_name_args *plugin_info,
	     struct plugin_gcc_version */*version*/)
{
  /* Read VALUE from -fplugin-arg-location_overflow_plugin-value=<VALUE>
     in hexadecimal form into base_location.  */
  for (int i = 0; i < plugin_info->argc; i++)
    {
      if (0 == strcmp (plugin_info->argv[i].key, "value"))
	base_location = strtol (plugin_info->argv[i].value, NULL, 16);
    }

  if (!base_location)
    error_at (UNKNOWN_LOCATION, "missing plugin argument");

  /* With 64-bit locations, the thresholds are larger, so shift the base
     location argument accordingly.  */
  gcc_assert (sizeof (location_t) == sizeof (uint64_t));
  base_location = 1 + ((base_location - 1) << 31);

  register_callback (plugin_info->base_name,
		     PLUGIN_PRAGMAS,
		     on_pragma_registration,
		     NULL); /* void *user_data */

  /* Hack in additional testing, based on the exact value supplied.  */
  original_text_finalizer = diagnostic_text_finalizer (global_dc);
  switch (base_location)
    {
    case LINE_MAP_MAX_LOCATION_WITH_PACKED_RANGES + 1:
      diagnostic_text_finalizer (global_dc) = verify_unpacked_ranges;
      break;

    case LINE_MAP_MAX_LOCATION_WITH_COLS + 1:
      diagnostic_text_finalizer (global_dc) = verify_no_columns;
      break;

    default:
      error_at (UNKNOWN_LOCATION, "unrecognized value for plugin argument");
    }

  return 0;
}
