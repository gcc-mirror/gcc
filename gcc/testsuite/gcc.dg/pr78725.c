/* { dg-do run } */
/* { dg-options "-O3 -fsplit-loops" } */

int fn1 (int b, int c)
{
    return c < 0 || c > 31 ? 0 : b >> c;
}

unsigned char d = 255; 
int e, f;

int main ()
{
  for (; f < 2; f++)
    e = fn1 (1, d++);
  if (e != 1)
    __builtin_abort ();
  return 0; 
}
