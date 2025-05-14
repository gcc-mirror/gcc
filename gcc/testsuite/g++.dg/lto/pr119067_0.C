/* { dg-lto-do link } */
/* { dg-skip-if "" { ! { x86_64-*-* i?86-*-* } } } */
/* { dg-require-effective-target avx2 } */
/* { dg-require-effective-target shared } */
/* { dg-lto-options { { -O2 -fPIC -flto } } } */
/* { dg-extra-ld-options { -shared } } */

#pragma GCC push_options
#pragma GCC target("avx2")
typedef char __v32qi __attribute__ ((__vector_size__ (32)));
struct ff
{
  __v32qi t;
};
__v32qi g(struct ff a);

__v32qi h(__v32qi a)
{
  struct ff t = {a};
  return g(t);
}
#pragma GCC pop_options
