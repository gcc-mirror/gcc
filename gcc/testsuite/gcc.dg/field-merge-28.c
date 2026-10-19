/* { dg-do run } */
/* { dg-options "-O1" } */

unsigned short b1, b2;
int c1 = 0, c2 = 0;

__attribute__((noipa)) void m1 () { c1++; }
__attribute__((noipa)) void m2 () { c2++; }

__attribute__((noipa))
void test1 ()
{
  for (int i = 0; i < 9; i++)
    {
      unsigned short val = (b1 ^= 65533);
      unsigned char k = val;
      if ((val & 240) || k)
	;
      else
	m1 ();
    }
}

__attribute__((noipa))
void test2 ()
{
  for (int i = 0; i < 9; i++)
    {
      unsigned short val = (b2 ^= 65533);
      signed char k = val;
      if ((val & 240) || k)
	;
      else
	m2 ();
    }
}

int main ()
{
  test1 ();
  if (c1 != 4)
    __builtin_abort ();
  test2 ();
  if (c2 != 4)
    __builtin_abort ();
  return 0;
}
