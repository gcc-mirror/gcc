/* { dg-do compile } */

int data[4096];

int
f (void)
{
  return (((unsigned int) &data[0]) == 0xdeadbea0U);
}

/* { dg-final { scan-assembler-not "\tmove\t\\\$2,\\\$0" } } */
