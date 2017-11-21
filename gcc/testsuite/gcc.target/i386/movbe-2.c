/* { dg-do compile } */
/* { dg-options "-O2 -mmovbe" } */

extern long long x;

void
foo (long long i)
{
  x = __builtin_bswap64 (i);
}

long long
bar ()
{
  return __builtin_bswap64 (x);
}

/* { dg-final { scan-assembler-times "movbel\[ \t\]" 4 { target ia32 } } } */
/* { dg-final { scan-assembler-times "movbeq\[ \t\]" 2 { target { ! ia32 } } } } */
