/* { dg-do compile } */
/* { dg-options "-O2 -fcheck-pointer-bounds -mmpx" } */

union jsval_layout
{
  void *asPtr;
};
union jsval_layout a;
union jsval_layout b;
union jsval_layout __inline__ fn1() { return b; }

void fn2() { a = fn1(); }
