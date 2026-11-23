/* PR target/128834 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -mtune=generic -march=x86-64 -fno-stack-protector -masm=att" } */
/* { dg-final { scan-assembler-times "\tmulq\t" 1 } } */
/* { dg-final { scan-assembler-times "\taddq\t" 1 } } */
/* { dg-final { scan-assembler-times "\tadcq\t" 2 } } */
/* { dg-final { scan-assembler-not "\tjn?\[cb]\t" } } */

void
foo (unsigned __int128 *x, unsigned long *y, unsigned long a, unsigned long b)
{
  unsigned __int128 p = (unsigned __int128) a * b;
  *x += p;
  *y += *x < p;
}
