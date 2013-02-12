/* PR target/56256 */
/* { dg-do assemble } */
/* { dg-options "-O2" } */

int
foo (void)
{
  int a;
  __asm__ ("{lil|li} %0,%1" : "=r" (a) : "I" (26));
  return a;
}
