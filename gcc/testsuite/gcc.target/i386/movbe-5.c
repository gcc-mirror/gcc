/* PR tree-optimization/78821 */
/* { dg-do compile } */
/* { dg-options "-O2 -mmovbe" } */
/* { dg-final { scan-assembler-times "movbe\[ \t\]" 2 } } */

unsigned short
foo (unsigned short *buf)
{
  unsigned short a = buf[0];
  return ((unsigned short) (a >> 8)) | (unsigned short) (a << 8);
}

void
bar (char *buf, unsigned int data)
{
  buf[0] = data >> 8;
  buf[1] = data;
}
