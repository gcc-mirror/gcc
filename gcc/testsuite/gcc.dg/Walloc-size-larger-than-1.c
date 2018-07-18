/* PR middle-end/82063 - issues with arguments enabled by -Wall
   { dg-do compile }
   { dg-options "-O -Walloc-size-larger-than=1KB -ftrack-macro-expansion=0" } */

void sink (void*);

#define T(x) sink (x)

void f (void)
{
  unsigned n = 0;
  T (__builtin_malloc (n));

  n = 1024;   /* 1 kibibyte (KB or KiB) */
  T (__builtin_malloc (n));

  n = 1025;
  T (__builtin_malloc (n));   /* { dg-warning "argument 1 value .1025. exceeds maximum object size 1024" } */
}
