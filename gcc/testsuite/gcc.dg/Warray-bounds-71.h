/* Verify that -Warray-bounds suppression via #pragma GCC diagnostic
   works at any call site in an inlining stack
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

int a[4];

void f1 (int *p, int i)
{
#pragma GCC diagnostic push
#if IGNORE == '1'
#  pragma GCC diagnostic ignored "-Warray-bounds"
#endif
  p[i + 1] = 0;
#pragma GCC diagnostic pop
}

void f2 (int *p, int i)
{
#pragma GCC diagnostic push
#if IGNORE == '2'
#  pragma GCC diagnostic ignored "-Warray-bounds"
#endif
  f1 (p + 1, i + 1);
#pragma GCC diagnostic pop
}

void f3 (int *p, int i)
{
#pragma GCC diagnostic push
#if IGNORE == '3'
#  pragma GCC diagnostic ignored "-Warray-bounds"
#endif
  f2 (p + 1, i + 1);
#pragma GCC diagnostic pop
}

void f4 (void)
{
#pragma GCC diagnostic push
#if IGNORE == '4'
#  pragma GCC diagnostic ignored "-Warray-bounds"
#endif
  f3 (a, 1);
#pragma GCC diagnostic pop
}
