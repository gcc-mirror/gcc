/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2" } */

void (__attribute__ ((stdcall)) *a) (int);

void __attribute__ ((stdcall))
foo (int x)
{
  a (x);
}

int (__attribute__ ((stdcall)) *b) (int);

int __attribute__ ((stdcall))
bar (int x)
{
  return b (x);
}
