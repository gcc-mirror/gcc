/* Test that builtin-macros are OK.  */

/* { dg-do run } */

#include "builtins.h"

void abort (void);
char *strstr (const char *, const char *); 
int strcmp (const char *, const char *);

#define LINE __LINE__

#if __LINE__ != 13
# error __LINE__ part 1  /* { dg-bogus "__LINE__" } */
#endif

#if \
  __LINE__ != 18
# error __LINE__ part 2  /* { dg-bogus "__LINE__" } */
#endif

#if LINE != 22
# error __LINE__ part 3  /* { dg-bogus "__LINE__" } */
#endif

#if __INCLUDE_LEVEL != 0
# error __INCLUDE_LEVEL__  /* { dg-bogus "__INCLUDE_LEVEL__" } */
#endif

#if !defined (__TIME__)
# error __TIME__  /* { dg-bogus "__TIME__" } */
#endif

#if !defined (__DATE__)
# error __DATE__  /* { dg-bogus "__DATE__" } */
#endif


int main ()
{
  /* level is defined in builtins.h.  */
  if (level != 1)
    abort ();

  if (!strstr (__FILE__, "builtins.c"))
    abort ();

  if (!strcmp (__BASE_FILE__, "builtins.c"))
    abort ();

  return 0;
}
