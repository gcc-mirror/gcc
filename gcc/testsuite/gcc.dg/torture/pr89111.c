/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

struct __attribute__((packed)) A {  int b : 24; } c[243], f;

int d, e, g, j;

__attribute__((noipa)) int
foo (int x)
{
  if (x != 0)
    __builtin_abort ();
  return 2;
}

int
main ()
{ 
  struct A h = f;
  h.b = 0;
  while (e++ < 3)
    { 
      while (d++ < 3)
	c[46].b ^= 9890739;
      f = c[46] = h;
    }
  while (g++ < 9)
    j = foo (c[g * 9 + j].b);
  return 0;
}
