/* { dg-do compile } */
/* { dg-options "" } */

void
foo (void)
{
  int i = 0;
  asm ("" : : : "memory");
  asm ("" : : : );
  asm ("" : : "r" (i));
  asm ("" : : );
  asm ("" : "=r" (i));
  asm ("" : );
  asm ("");
}
