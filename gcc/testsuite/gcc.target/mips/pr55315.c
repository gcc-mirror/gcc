/* { dg-do compile } */

int data[4096];

int
f (void)
{
  return (((unsigned long) &data[0]) == 0xdeadbea0U);
}

/* { dg-final { scan-assembler-not "\tmove\t\\\$2,\\\$0" } } */
