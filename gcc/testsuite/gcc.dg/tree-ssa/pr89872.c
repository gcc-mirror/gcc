/* PR c/89872 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times " ={v} 1;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " ={v} 2;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " ={v} 3;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " ={v} 4;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " ={v} 0;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " ={v} " 10 "optimized" } } */

void
foo (void)
{
  (volatile int){1} + (volatile int){2};
}

void
bar (void)
{
  (volatile int){3};
}

void
baz (void)
{
  (volatile int){4} / (volatile int){0};
}
