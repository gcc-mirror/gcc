/* { dg-do run } */

struct S
{
  int b:4;
  int c; 
} e = { -1, 0 };

int d, f;

int main ()
{
  while (f)
    {
      struct S g = { 0, 0 };
      e = g;
    }
L:
  while (e.b > 0)
    ;
  e.b = 0;
  if (d)
    goto L;
  return 0;
}
