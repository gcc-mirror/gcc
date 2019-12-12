/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler "and\[lq\]?\[\\t \]*\\$-64,\[\\t \]*%\[re\]?sp" } } */
/* { dg-skip-if "" { x86_64-*-mingw* } } */

typedef int __v16si __attribute__ ((__vector_size__ (64)));

extern void foo (__v16si, __v16si, __v16si, __v16si, __v16si, __v16si,
		 __v16si, __v16si, __v16si, int, int, int, int, int,
		 int, __v16si *);

extern __v16si x, y;

void
bar (void)
{
  foo (x, x, x, x, x, x, x, x, x, 0, 1, 2, 3, 4, 5, &y);
}
