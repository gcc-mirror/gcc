/* PR optimization/11381 */
/* Originator: <tobias@ringstrom.mine.nu> */
/* { dg-do compile } */
/* { dg-options "-O" } */

/* Verify that the comparison is not optimized away. */

void foo(volatile unsigned int *vaddr)
{
  while (*vaddr != *vaddr)
    ;
}

/* { dg-final { scan-assembler "cmp" } } */
