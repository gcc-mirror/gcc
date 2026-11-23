/* { dg-do compile } */
/* { dg-options "-O2 -mavx2 -mno-avx512f" } */

/* The stored address is defined from the stack pointer before the loop,
   but only the loop-carried "p -= 32" or "++p" reaches the store, so the
   stack address is reached transitively.  The stack must still be
   realigned.  */

typedef int v8si __attribute__ ((vector_size (32)));

void
f1 (long n, v8si x)
{
  char *p = (char *) __builtin_stack_address () - 64;
  do
    {
      p -= 32;
      *(v8si *) p = x;
    }
  while (--n);
}

void
f2 (long n, v8si x)
{
  v8si *p = (v8si *) ((char *) __builtin_stack_address () - 1024);
  for (long i = 0; i < n; i++)
    {
      ++p;
      p[-1] = x;
    }
}

/* { dg-final { scan-assembler-times "and\[lq\]?\[^\\n\]*-32,\[^\\n\]*sp" 2 } } */
