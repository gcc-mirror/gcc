/* { dg-do run } */

int a, c;

int main ()
{
  int e = -1;
  short d = (c <= 0) ^ e;
  if ((unsigned int) a - (a || d) <= (unsigned int) a)
    __builtin_abort ();
  return 0;
}
