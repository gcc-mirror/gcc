/* { dg-do run } */
/* { dg-options "-mstringop-strategy=unrolled_loop" } */

char c[2841];

__attribute__((noipa))
void
foo (void)
{
  __builtin_memset (&c, 1, 2841);
}

int
main (void)
{
  foo ();
  for (unsigned i = 0; i < sizeof (c); i++)
    if (c[i] != 1)
      __builtin_abort();
  return 0;
}
