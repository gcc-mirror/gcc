/* PR target/94913 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

char fooc (unsigned char x, unsigned char y)
{
  return (unsigned char) ~x < y;
}

short foos (unsigned short x, unsigned short y)
{
  return (unsigned short) ~x < y;
}

long fooi (unsigned long x, unsigned long y)
{
  return (unsigned long) ~x < y;
}

/* { dg-final { scan-assembler-not "cmp" } } */
/* { dg-final { scan-assembler-times "add" 3 } } */
