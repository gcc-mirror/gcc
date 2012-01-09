/* PR preprocessor/pr33919 */
/* { dg-do run } */
/* { dg-options "-I . -include ${srcdir}/gcc.dg/pr33919-0.h" } */

#include "pr33919-1.h"

const char *base_file = __BASE_FILE__;

extern int strcmp (const char *, const char *);
extern void abort (void);

int
main ()
{
  if (!strcmp (pre_inc_base_file, "<command line>"))
    abort ();
  if (strcmp (pre_inc_base_file, __FILE__))
    abort ();
  if (strcmp (base_file, __FILE__))
    abort ();
  if (strcmp (inc_base_file, __FILE__))
    abort ();
  if (strcmp (nested_inc_base_file, __FILE__))
    abort ();
  return 0;
}
