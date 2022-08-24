/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2" } */

unsigned long long bar();

unsigned long long foo()
{
  unsigned long long x = bar();
  return (x>>32) | (x<<32);
}

/*{ dg-final { scan-assembler "xchgl" } } */
