/* PR optimization/8746 */
/* { dg-do run } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O1 -mtune=i586" } */

extern void abort (void);

unsigned short r0;

int foo(int x)
{
  unsigned short r = x&0xf000;

  if (!(r&0x8000))
  {
    r0 = r;
    return 0;
  }
  else
    return 1;
}

int main(void)
{
  if (foo(0x8000) != 1)
    abort();

   return 0;
}
