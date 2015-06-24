/* Check calling convention in the vector ABI.  */

/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13" } */

/* { dg-final { scan-assembler "gnu_attribute 8, 2" } } */

typedef int __attribute__((vector_size(16))) v4si;

extern void bar (v4si);

void
foo (int a)
{
  v4si b = (v4si){ a, a, a, a };
  bar (b);
}
