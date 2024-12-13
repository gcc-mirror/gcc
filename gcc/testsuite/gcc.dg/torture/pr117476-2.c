/* PR rtl-optimization/117476.
   Second case checking skipping of TI mode. */
/* { dg-do run } */
/* { dg-require-effective-target int128 } */

unsigned __int128 g;

void
foo ()
{
  g += __builtin_add_overflow_p (~g, 0, 0ul);
}

int
main ()
{
  foo();
  if (!g)
    __builtin_abort();
}
