/* PR rtl-optimization/52139 */
/* { dg-do compile } */
/* { dg-options "-O -fno-tree-dominator-opts -fno-tree-fre" } */
/* { dg-additional-options "-fpic" { target fpic } } */

void *p;

void
foo (int a)
{
  switch (a)
    {
    case 0:
    a0:
    case 1:
    a1:
      p = &&a1;
    case 2:
    a2:
      p = &&a2;
    case 3:
    a3:
      p = &&a3;
    case 4:
    a4:
      p = &&a4;
    case 5:
    a5:
      p = &&a5;
    case 6:
    a6:
      p = &&a6;
    case 7:
    a7:
      p = &&a7;
    case 8:
    a8:
      p = &&a8;
    case 9:
    a9:
      p = &&a9;
    case 10:
    a10:
      p = &&a10;
    default:
      p = &&a0;
    }
  goto *p;
}
