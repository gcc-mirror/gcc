/* PR middle-end/82063 - issues with arguments enabled by -Wall
   { dg-do compile }
   { dg-options "-O -Wno-alloc-size-larger-than -ftrack-macro-expansion=0" } */

typedef __SIZE_TYPE__ size_t;

void sink (void*);

#define T(x) sink (x)

void f (void)
{
  size_t n = 0;
  T (__builtin_malloc (n));

  n = __PTRDIFF_MAX__;
  T (__builtin_malloc (n));

  n += 1;
  T (__builtin_malloc (n));

  n = __SIZE_MAX__ - 1;
  T (__builtin_malloc (n));

  n = __SIZE_MAX__;
  T (__builtin_malloc (n));
}
