/* { dg-do compile } */
/* { dg-options "-Walloca-larger-than=128 -O2" } */

typedef __SIZE_TYPE__ size_t;

void bar (void*);

void foo1 (size_t len)
{
  bar (__builtin_alloca_with_align_and_max (len, 8, 128));
}

void foo2 (size_t len)
{
  bar (__builtin_alloca_with_align_and_max (len, 8, 256)); /* { dg-warning "may be too large" } */
}
