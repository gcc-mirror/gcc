/* { dg-do run { target lp64 } } */
/* This is a test exercising peeling for alignment for a positive step
   vector loop.  We're forcing atom tuning here because that has a higher
   unaligned vs aligned cost unlike most other archs.  */
/* { dg-options "-O3 -march=x86-64 -mtune=atom -fdump-tree-vect-details -save-temps" } */

float a[1024], b[1024];

void __attribute__((noipa)) foo1 ()
{
  for (int i = 2; i < 508; ++i)
    a[i] = b[i] * 2.;
}
void __attribute__((noipa)) foo2 ()
{
  for (int i = 3; i < 508; ++i)
    a[i] = b[i] * 2.;
}
void __attribute__((noipa)) foo3 ()
{
  for (int i = 4; i < 508; ++i)
    a[i] = b[i] * 2.;
}
void __attribute__((noipa)) foo4 ()
{
  for (int i = 5; i < 508; ++i)
    a[i] = b[i] * 2.;
}
void __attribute__((noipa)) foo5 (int start)
{
  for (int i = start; i < 508; ++i)
    a[i] = b[i] * 2.;
}

int main()
{
  for (int i = 2; i < 508; ++i)
    {
      __asm__ volatile ("" : : : "memory");
      b[i] = i;
    }
  foo1 ();
#pragma GCC novector
  for (int i = 2; i < 508; ++i)
    if (a[i] != 2*i)
      __builtin_abort ();

  for (int i = 3; i < 508; ++i)
    {
      __asm__ volatile ("" : : : "memory");
      b[i] = i;
    }
  foo2 ();
#pragma GCC novector
  for (int i = 3; i < 508; ++i)
    if (a[i] != 2*i)
      __builtin_abort ();

  for (int i = 4; i < 508; ++i)
    {
      __asm__ volatile ("" : : : "memory");
      b[i] = i;
    }
  foo3 ();
#pragma GCC novector
  for (int i = 4; i < 508; ++i)
    if (a[i] != 2*i)
      __builtin_abort ();

  for (int i = 5; i < 508; ++i)
    {
      __asm__ volatile ("" : : : "memory");
      b[i] = i;
    }
  foo4 ();
#pragma GCC novector
  for (int i = 5; i < 508; ++i)
    if (a[i] != 2*i)
      __builtin_abort ();

  for (int i = 3; i < 508; ++i)
    {
      __asm__ volatile ("" : : : "memory");
      b[i] = i;
    }
  foo5 (3);
#pragma GCC novector
  for (int i = 3; i < 508; ++i)
    if (a[i] != 2*i)
      __builtin_abort ();
}

/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 4 "vect" } } */ 
/* Verify all vector accesses are emitted as aligned.  */
/* { dg-final { scan-assembler-not "movup" } } */
