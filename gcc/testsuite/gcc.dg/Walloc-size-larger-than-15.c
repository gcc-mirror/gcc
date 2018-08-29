/* PR middle-end/82063 - issues with arguments enabled by -Wall
   { dg-do compile }
   { dg-options "-O -Walloc-size-larger-than=123456789123456789123456789123456789gb -ftrack-macro-expansion=0" } */

typedef __SIZE_TYPE__ size_t;

void sink (void*);

#define T(x) sink (x)

/* Verify that an exceeingly large -Walloc-size-larger-than argument
   with a valid suffic is accepted and treated as infinite.  */

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
