/* Simplified version of test to ensure we issue a FILE * leak diagnostic,
   reproducing a feasibility issue.
   Adapted from intl/localealias.c, with all #includes removed.  */

/* { dg-do "compile" } */

#include "analyzer-decls.h"

#define NULL ((void *) 0)
#define PATH_SEPARATOR ':'
#define LOCALE_ALIAS_PATH "value for LOCALE_ALIAS_PATH"

const char *
_nl_expand_alias (void)
{
  static const char *locale_alias_path;

  if (locale_alias_path == NULL)
    locale_alias_path = LOCALE_ALIAS_PATH;

  const char *start = locale_alias_path;

  while (locale_alias_path[0] != '\0'
	 && locale_alias_path[0] != PATH_SEPARATOR)
    ++locale_alias_path;

  if (start < locale_alias_path)
    __analyzer_dump_path (); /* { dg-message "path" } */
}
