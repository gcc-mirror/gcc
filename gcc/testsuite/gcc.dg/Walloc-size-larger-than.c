/* PR middle-end/82063 - issues with arguments enabled by -Wall
   { dg-do compile }
   { dg-options "-Walloc-size-larger-than=0 -ftrack-macro-expansion=0" } */

void sink (void*);

#define T(x) sink (x)

void f (void)
{
  T (__builtin_malloc (0));
  T (__builtin_malloc (1));   /* { dg-warning "argument 1 value .1. exceeds maximum object size 0" } */
}
