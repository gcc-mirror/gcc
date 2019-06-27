/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler-not "and\[lq\]?\[\\t \]*\\$-64,\[\\t \]*%\[re\]?sp" } } */

typedef int __v16si __attribute__ ((__vector_size__ (64)));

void
foo (__v16si x, int i0, int i1, int i2, int i3, int i4, int i5, __v16si *p)
{
  *p = x;
}
