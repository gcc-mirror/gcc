/* PR gcov-profile/52150 */
/* { dg-options "-O0" } */

void foo () __asm__ ("bar");

void
foo ()
{
}

int
main ()
{
  foo ();
  return 0;
}
