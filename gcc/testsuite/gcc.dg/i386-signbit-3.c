/* PR optimization/8746 */
/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O1 -mtune=i586" } */

extern void abort (void);

volatile int j;

void f0() { j=0; }
void f1() { j=1; }

int foo(int x)
{
  if ((short int)(x&0x8000) > (short int)0)
  {
    f0();
    return 0;
  }
  else
  {
    f1();
    return 1;
  }
}

int main(void)
{
  if (foo(0x8000) != 1)
    abort();

   return 0;
}
