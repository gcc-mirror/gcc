/* PR middle-end/82063 - issues with arguments enabled by -Wall
   { dg-do compile }
   { dg-require-effective-target alloca }
   { dg-options "-O2 -Walloca-larger-than=0 -Wvla-larger-than=0 -ftrack-macro-expansion=0" } */

extern void* alloca (__SIZE_TYPE__);

void sink (void*);

#define T(x) sink (x)

void test_alloca (void)
{
  /* Verify that alloca(0) is diagnosed even if the limit is zero.  */
  T (alloca (0));   /* { dg-warning "argument to .alloca. is zero" } */
  T (alloca (1));   /* { dg-warning "argument to .alloca. is too large" } */
}

void test_vla (unsigned n)
{
  /* VLAs smaller than 32 bytes are optimized into ordinary arrays.  */
  if (n < 1 || 99 < n)
    n = 1;

  char a[n];        /* { dg-warning "argument to variable-length array " } */
  T (a);
}
