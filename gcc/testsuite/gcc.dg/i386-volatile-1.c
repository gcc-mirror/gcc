/* PR optimization/11381 */
/* Originator: <tobias@ringstrom.mine.nu> */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O" } */

/* Verify that the comparison is not optimized away. */

void foo(volatile unsigned int *vaddr)
{
  while (*vaddr != *vaddr)
    ;
}

/* { dg-final { scan-assembler "cmp" } } */
