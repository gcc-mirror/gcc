/* Test that we don't lose side-effects when converting from nullptr_t.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -pedantic-errors" } */

int i;
nullptr_t fn () { ++i; return nullptr; }

int
main ()
{
  int *p = fn ();
  if (i != 1)
    __builtin_abort ();
}
