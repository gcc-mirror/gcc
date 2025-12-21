/* Fix for PR123089 alignment peeling with vectors and VLS and overflows.  */
/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-Ofast --param aarch64-autovec-preference=sve-only" } */
/* { dg-additional-options "-msve-vector-bits=256" { target aarch64_sve256_hw } } */
/* { dg-additional-options "-msve-vector-bits=128" { target aarch64_sve128_hw } } */

#define START 2

int __attribute__((noipa))
foo (unsigned char n, int *x)
{
  unsigned char i = 0;
#pragma GCC unroll 0
  for (i = START; i < n; ++i)
    {
      if (x[i] == 0)
        return i;
      x[i] += 1;
    }
  return i;
}

int main ()
{
   int max = 255 - START;
   int x[255 - START];
#pragma GCC unroll 0
   for (int i = 0; i < max; i++)
     x[i] = 1;

   x[200] = 0;
   int res = foo (max, x);
   if (res != 200)
     __builtin_abort ();

   if (x[START] != 2)
     __builtin_abort ();

   if (x[0] != 1)
     __builtin_abort ();
   return 0;
}
