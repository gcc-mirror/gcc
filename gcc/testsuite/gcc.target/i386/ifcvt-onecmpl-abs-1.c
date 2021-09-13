/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Check code generation for one's complement version of abs */

int onecmplabs(int x)
{
  if (x < 0)
    x = ~x;
  return x;
}

/* { dg-final { scan-assembler "\txor" } } */
