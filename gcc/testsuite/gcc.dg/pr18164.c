/* { dg-do compile } */
/* { dg-options "" } */
void
f (void)
{
  int x;
  asm ("" :  "" (x)); /* { dg-error "output operand constraint lacks" } */
}
