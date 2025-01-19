/* { dg-do compile } */
/* { dg-options "-Os -fdump-tree-optimized" } */

char volatile v;
void for16 (void)
{
  for (char i = 16; i > 0; i -= 2)
    v = i;
}

/* { dg-final { scan-tree-dump-times " ={v} " 1 "optimized" } } */
