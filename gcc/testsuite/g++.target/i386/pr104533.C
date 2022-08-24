/* PR ipa/104533 */
/* { dg-do compile } */
/* { dg-additional-options "-std=c++11 -fPIC -Ofast -fno-semantic-interposition" } */
/* { dg-require-ifunc "" } */

struct B
{
  virtual ~B();
};
__attribute__((target_clones("avx")))
B::~B() = default; /* { dg-warning "single .target_clones. attribute is ignored" } */
