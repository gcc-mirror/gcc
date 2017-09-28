/* { dg-do compile } */
/* { dg-options "-mstrict-align -O3" } */

unsigned char foo(const unsigned char *buffer, unsigned int length)
{
  unsigned char sum;
  unsigned int  count;

  for (sum = 0, count = 0; count < length; count++) {
    sum = (unsigned char) (sum + *(buffer + count));
  }

  return sum;
}

/* { dg-final { scan-assembler-times "and\tw\[0-9\]+, w\[0-9\]+, 15" 1 } } */
