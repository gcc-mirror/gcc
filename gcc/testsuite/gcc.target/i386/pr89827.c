/* PR target/89827 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -msse2 -mstv -mno-stackrealign" } */

unsigned long long a;

void
foo (void)
{
  a >>= (unsigned short) a;
}
