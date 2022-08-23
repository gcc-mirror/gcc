/* { dg-do compile } */
/* { dg-options "-Os -fdump-tree-optimized -save-temps" } */

int
f (char a)
{
  return  __builtin_memchr ("ac", a, 1) == 0;
}

/* { dg-final { scan-assembler "memchr" } } */
