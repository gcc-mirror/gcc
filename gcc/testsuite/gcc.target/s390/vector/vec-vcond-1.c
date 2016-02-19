/* A const vector operand is forced into a register in
   s390_expand_vcond.
   This testcase once failed because the target mode (v2di) was picked
   for the reg instead of the mode of the other comparison
   operand.  */

/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13" } */

typedef __attribute__((vector_size(16))) long   v2di;
typedef __attribute__((vector_size(16))) double v2df;

v2di
foo (v2df a)
{
  return a == (v2df){ 0.0, 0.0 };
}

v2di
bar (v2df a)
{
  return (v2df){ 1.0, 1.0 } == (v2df){ 0.0, 0.0 };
}
