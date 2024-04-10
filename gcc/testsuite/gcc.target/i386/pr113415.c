/* PR middle-end/113415 */
/* { dg-do compile } */
/* { dg-options "-mstringop-strategy=byte_loop" } */

void
foo (void)
{
  unsigned long arr[64];
lab:
  __asm__ goto ("" : "=r" (arr) : : : lab);	/* { dg-error "impossible constraint in 'asm'" } */
}
