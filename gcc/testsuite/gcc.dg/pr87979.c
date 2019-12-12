/* PR rtl-optimization/87979 */
/* { dg-do compile } */
/* { dg-options "-Os -fmodulo-sched -fno-tree-loop-im" } */
/* { dg-additional-options "-march=z196" { target { s390*-*-* } } } */

void foo(void)
{
  static int m;
  for (int i = 0; i < 10; ++i)
    m++;
}
