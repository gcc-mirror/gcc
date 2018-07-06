/* PR inline-asm/84742 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void 
foo ()
{
  char b = 1;
  asm volatile ("" : "+gT,m" (b));	/* { dg-error "impossible constraint in 'asm'" } */
}
