// PR optimization/6759
// This testcase ICEd on SPARC because folded REG_EQUAL
// note was note stored back and fold_rtx left invalid rtx
// in it.
// { dg-do compile }
// { dg-options "-O2" }

struct A
{
  long long a;
  A (unsigned short d) : a (d) {}
} x (65535);
