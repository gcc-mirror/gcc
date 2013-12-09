/* PR rtl-optimization/59020 */

/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fmodulo-sched -fno-inline -march=corei7" } */

int a, b, d;
unsigned c;

void f()
{
  unsigned q;
  for(; a; a++)
    if(((c %= d && 1) ? : 1) & 1)
      for(; b; q++);
}
