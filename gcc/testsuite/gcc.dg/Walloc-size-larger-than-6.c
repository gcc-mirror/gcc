/* PR middle-end/82063 - issues with arguments enabled by -Wall
   { dg-do compile }
   { dg-options "-O -Walloc-size-larger-than=1GiB -ftrack-macro-expansion=0" } */

void sink (void*);

#define T(x) sink (x)

void f (void)
{
  __SIZE_TYPE__ n = 0;
  T (__builtin_malloc (n));

  n = 1024 * 1024 * 1024;   /* 1 gigibyte (GiB) */
  T (__builtin_malloc (n));

  n += 1;
  T (__builtin_malloc (n));   /* { dg-warning "argument 1 value .1073741825. exceeds maximum object size 1073741824" } */

  n = __PTRDIFF_MAX__;
  T (__builtin_malloc (n));   /* { dg-warning "exceeds maximum object size" } */

  n = __SIZE_MAX__;
  T (__builtin_malloc (n));   /* { dg-warning "exceeds maximum object size" } */
}
