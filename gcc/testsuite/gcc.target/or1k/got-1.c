/* { dg-do compile } */
/* { dg-options "-O2 -fPIC -mcmodel=large" } */

/* Generate references to the GOT. */

extern int geti (void);
extern int j;

int
calc (int a)
{
  return a * j + geti ();
}

/* Ensure the 2 references use gotha relocations and that the function call does
   not use an immediate jump instruction.  */
/* { dg-final { scan-assembler-times "gotha" 2 } } */
/* { dg-final { scan-assembler "l.jalr\\s+" } } */
