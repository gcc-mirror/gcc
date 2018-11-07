/* PR middle-end/82063 - issues with arguments enabled by -Wall
   Verify that alloca() calls in loops are not diagnosed by default.
   { dg-do compile }
   { dg-options "-O2 -ftrack-macro-expansion=0" } */

extern void* alloca (__SIZE_TYPE__);

void sink (void*);

#define T(x) sink (x)

void test_alloca (unsigned n)
{
  /* Verify that alloca(0) is not diagnosed in a loop either.  */
  for (unsigned i = 0; i < n; ++i)
    T (alloca (0));

  /* Verify no warnings for the loops below.  */
  for (unsigned i = 0; i < n; ++i)
    T (alloca (1));

  for (unsigned i = 1; i < n; ++i)
    T (alloca (n));
}
