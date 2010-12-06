/* PR target/43897 */
/* { dg-do assemble } */
/* { dg-options "-O2" } */

int
sub (int i)
{
  float tmp;
  if (i)
    __asm__ __volatile__ ("frcpa.s0 %0,p1=f0,f0":"=f" (tmp)::"p1");
  return i + 10;
}
