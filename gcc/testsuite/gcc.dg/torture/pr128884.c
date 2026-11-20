/* { dg-do run } */

unsigned char a;

unsigned long __attribute__((noipa))
f (unsigned long x)
{
  if (a >= 7)
    {
      unsigned char t = a * 2;
      x = (x * (unsigned long) a) ^ t;
    }
  else
    x += (unsigned long) a * 2;
  return x;
}

int
main ()
{
  a = 200;
  if (f (0) != 144)
    __builtin_abort ();
  return 0;
}
