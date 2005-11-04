/* PR optimization/6842
   This testcase caused ICE when trying to optimize V8QI subreg of VOIDmode
   CONST_DOUBLE.  */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -mmmx" } */

typedef unsigned char __v8qi __attribute__ ((vector_size (8)));
extern void abort (void);
extern void exit (int);

void foo (void)
{
  unsigned long long a = 0x0102030405060708LL;
  unsigned long long b = 0x1020304050607080LL;
  unsigned long long c;

  c = (unsigned long long) __builtin_ia32_paddusb ((__v8qi) a, (__v8qi) b);
  __builtin_ia32_emms ();
  if (c != 0x1122334455667788LL)
    abort ();
}
