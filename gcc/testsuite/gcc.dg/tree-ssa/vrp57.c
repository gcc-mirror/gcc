/* PR40052 */
/* { dg-do compile } */
/* { dg-options "-O -ftree-vrp -fno-tree-ccp -fdump-tree-optimized" } */

int foo(_Bool b)
{
    return b | 1;
}

int bar(_Bool b)
{
    return b & -2;
}

/* { dg-final { scan-tree-dump "return 0;" "optimized" } } */
/* { dg-final { scan-tree-dump "return 1;" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
