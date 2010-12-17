/* { dg-do compile } */
/* { dg-options "-O -fno-tree-dce" } */

void foo (int i)
{
  __builtin_expect (i, i);
}

