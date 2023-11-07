/* Test bool, true and false keywords in C23.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -pedantic-errors" } */

extern void abort (void);
extern void exit (int);

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

extern bool b;
extern _Bool b;

_Static_assert (false == 0);
_Static_assert (true == 1);

int
main (void)
{
  if (_Generic (true, bool : 1) != 1)
    abort ();
  if (true != 1)
    abort ();
  if (_Generic (false, bool : 1) != 1)
    abort ();
  if (false != 0)
    abort ();
  exit (0);
}
