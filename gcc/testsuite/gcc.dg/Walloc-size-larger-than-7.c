/* PR middle-end/82063 - issues with arguments enabled by -Wall
   { dg-do compile }
   { dg-options "-O -Walloc-size-larger-than=1GB -ftrack-macro-expansion=0" } */

void sink (void*);

#define T(x) sink (x)

void f (void)
{
  __SIZE_TYPE__ n = 0;
  T (__builtin_malloc (n));

  n = 1000 * 1000 * 1000;   /* 1 gigabyte (GB) */
  T (__builtin_malloc (n));

  n += 1;
  T (__builtin_malloc (n));   /* { dg-warning "argument 1 value .1000000001. exceeds maximum object size 1000000000" } */

  n = __PTRDIFF_MAX__;
  T (__builtin_malloc (n));   /* { dg-warning "exceeds maximum object size" } */

  n = __SIZE_MAX__;
  T (__builtin_malloc (n));   /* { dg-warning "exceeds maximum object size" } */
}
