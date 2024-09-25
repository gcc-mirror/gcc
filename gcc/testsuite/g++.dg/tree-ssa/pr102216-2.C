/* { dg-options "-O2 -Wall" } */
/* { dg-skip-if "requires hosted libstdc++ for cstddef NULL" { ! hostedlib } } */

#include <algorithm>

static inline bool
compare_cstrings (const char *str1, const char *str2)
{
  return str1 < str2;
}

void
add_set_language_command ()
{
  static const char **language_names;

  language_names = new const char *[6];

  language_names[0] = "auto";
  language_names[1] = "local";
  language_names[2] = "unknown";

  const char **language_names_p = language_names;
  /* language_names_p == &language_names[0].  */
  language_names_p++;
  /* language_names_p == &language_names[1].  */
  language_names_p++;
  /* language_names_p == &language_names[2].  */
  language_names_p++;
  /* language_names_p == &language_names[3].  */

  const char **sort_begin;

  if (0)
    sort_begin = &language_names[3];
  else
    sort_begin = language_names_p;

  language_names[3] = "";
  language_names[4] = "";
  language_names[5] = NULL;

  /* There should be no warning associated with this std::sort as
     sort_begin != &language_names[5] and GCC should be able to figure
     that out.  */
  std::sort (sort_begin, &language_names[5], compare_cstrings);
}
