/* { dg-do compile } */
/* { dg-options "-O2 -fcheck-pointer-bounds -mmpx -march=corei7-avx" } */

struct c1
{
  c1 (const c1 &other) : p (other.p) { };
  int *p;
};

struct c2 : public c1 { };

c1
test (c2 a)
{
  return a;
}
