/* PR optimization/11018 */
/* Originator: <partain@dcs.gla.ac.uk> */

/* { dg-do run } */
/* { dg-require-effective-target ultrasparc_hw } */
/* { dg-options "-O2 -mcpu=ultrasparc" } */

/* This used to fail on 32-bit Ultrasparc because
   of broken DImode shift patterns.  */

extern void abort(void);

typedef unsigned long long uint64_t;
typedef unsigned int size_t;


void to_octal (uint64_t value, char *where, size_t size)
{
  uint64_t v = value;
  size_t i = size;

  do
    {
      where[--i] = '0' + (v & ((1 << 3) - 1));
      v >>= 3;
    }
  while (i);
}


int main (void)
{
  char buf[8];

  to_octal(010644, buf, 6);

  if (buf[1] != '1')
     abort();

  return 0;
}
