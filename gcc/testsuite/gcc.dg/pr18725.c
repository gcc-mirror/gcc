/* { dg-do compile } */
/* { dg-options "" } */

void
f (void)
{
   int x = 1;
   asm ("" : "" (x), "" (x)); /* { dg-error "output operand constraint lacks" } */
}
