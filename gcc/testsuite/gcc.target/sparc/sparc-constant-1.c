/* PR optimization/10876 */

/* { dg-do compile } */

/* Verify that adding the constant 4096 is turned
   into subtracting the constant -4096. */

int foo(int a)
{
  return a+4096;
}

/* { dg-final { scan-assembler "sub" } } */
