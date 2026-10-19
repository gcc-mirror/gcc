/* { dg-do run } */

int b, c;
int d (int *p1)
{
  switch ((unsigned char)*p1)
  case 1:
  case 3:
  case 255:
    return 1 + b % 60;
  return 0;
}
int main ()
{
  int e[] = {255};
  c = d(e);
  if (c != 1)
    __builtin_abort ();
  return 0;
}
