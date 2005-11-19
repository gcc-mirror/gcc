/* PR target/6054 */
/* { dg-do compile } */
/* { dg-options "-O -mconstant-gp" } */
/* { dg-final { scan-assembler "mov r1 =" } } */

extern void direct (void);
void foo(void (*indirect) (void))
{
  indirect ();
  direct ();
}
