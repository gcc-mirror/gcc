/* Test macro expansions in <stdbool.h> in C11.  */
/* { dg-do run } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#include <stdbool.h>

#define str(x) xstr(x)
#define xstr(x) #x

extern void abort (void);
extern void exit (int);
extern int strcmp (const char *, const char *);

#if false - 1 >= 0
#error "false unsigned in #if"
#endif

#if false != 0
#error "false not 0 in #if"
#endif

#if true - 2 >= 0
#error "true unsigned in #if"
#endif

#if true != 1
#error "true not 1 in #if"
#endif

int
main (void)
{
  if (strcmp (str (bool), "_Bool") != 0)
    abort ();
  if (_Generic (true, int : 1) != 1)
    abort ();
  if (true != 1)
    abort ();
  if (strcmp (str (true), "1") != 0)
    abort ();
  if (_Generic (false, int : 1) != 1)
    abort ();
  if (false != 0)
    abort ();
  if (strcmp (str (false), "0") != 0)
    abort ();
  if (strcmp (str (__bool_true_false_are_defined), "1") != 0)
    abort ();
  exit (0);
}
