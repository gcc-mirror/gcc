/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-fdump-tree-original" } */

void foo (int *p, int x)
{
  if ((x & 0xff) <= 7)
    *p = 0;
}

void bar (int *p, int x)
{
  if ((x & 0xff) < 8)
    *p = 0;
}

/* { dg-final { scan-tree-dump-times "(x & .*) == 0" 2 "original" } } */
