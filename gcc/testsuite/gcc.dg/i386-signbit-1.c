/* PR optimization/8746 */
/* { dg-do run { target i?86-*-* } } */
/* { dg-options "-O1 -mcpu=i586" } */

extern void abort (void);

unsigned char r0;

int foo(int x)
{
  unsigned char r = x&0xf0;

  if (!(r&0x80))
  {
    r0 = r;
    return 0;
  }
  else
    return 1;
}

int main(void)
{
  if (foo(0x80) != 1)
    abort();

   return 0;
}
