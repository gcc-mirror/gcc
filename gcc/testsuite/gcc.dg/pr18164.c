/* { dg-do compile } */
void
f (void)
{
  int x;
  asm ("" :  "" (x)); /* {dg-error "output operand constraint lacks" } */
}
