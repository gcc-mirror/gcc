/* PR c/78973 - warning: ‘memcpy’: specified size exceeds maximum object size

   Test case for what was initially thought to be a false positive but after
   deeper investigation turned out to be a true positive.

   { dg-do compile }
   { dg-options "-O2 -Wall" } */

static void f (void *p, int n)
{
  if (n <= 4)
    __builtin_memset (p, 0, n);   /* { dg-warning "exceeds maximum object size" "pr79073" { xfail { ilp32 || { int16 && { ! msp430_large } } } } } */
}

void g (void *d, unsigned n)
{
  if (n < 5)
    n = 5;
  f (d, n);
}
