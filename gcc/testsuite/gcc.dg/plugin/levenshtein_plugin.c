/* Plugin for unittesting gcc/spellcheck.h.  */

#include "config.h"
#include "gcc-plugin.h"
#include "system.h"
#include "coretypes.h"
#include "spellcheck.h"
#include "diagnostic.h"

int plugin_is_GPL_compatible;

static void
levenshtein_distance_unit_test_oneway (const char *a, const char *b,
				       edit_distance_t expected)
{
  edit_distance_t actual = levenshtein_distance (a, b);
  if (actual != expected)
    error ("levenshtein_distance (\"%s\", \"%s\") : expected: %i got %i",
	   a, b, expected, actual);
}


static void
levenshtein_distance_unit_test (const char *a, const char *b,
				edit_distance_t expected)
{
  /* Run every test both ways to ensure it's symmetric.  */
  levenshtein_distance_unit_test_oneway (a, b, expected);
  levenshtein_distance_unit_test_oneway (b, a, expected);
}

/* Callback handler for the PLUGIN_FINISH event; run
   levenshtein_distance unit tests here.  */

static void
on_finish (void */*gcc_data*/, void */*user_data*/)
{
  levenshtein_distance_unit_test ("", "nonempty", strlen ("nonempty"));
  levenshtein_distance_unit_test ("saturday", "sunday", 3);
  levenshtein_distance_unit_test ("foo", "m_foo", 2);
  levenshtein_distance_unit_test ("hello_world", "HelloWorld", 3);
  levenshtein_distance_unit_test
    ("the quick brown fox jumps over the lazy dog", "dog", 40);
  levenshtein_distance_unit_test
    ("the quick brown fox jumps over the lazy dog",
     "the quick brown dog jumps over the lazy fox",
     4);
  levenshtein_distance_unit_test
    ("Lorem ipsum dolor sit amet, consectetur adipiscing elit,",
     "All your base are belong to us",
     44);
}

int
plugin_init (struct plugin_name_args *plugin_info,
	     struct plugin_gcc_version */*version*/)
{
  register_callback (plugin_info->base_name,
		     PLUGIN_FINISH,
		     on_finish,
		     NULL); /* void *user_data */

  return 0;
}
