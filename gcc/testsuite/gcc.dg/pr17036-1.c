/* { dg-do compile } */

int main ()
{
  int R, N = 4;
  unsigned int A = 2;
  signed int B = 2;
  ((B >> N) & 1) ? 1 : 0;
  ((A >> N) & 1) ? 1 : 0;
  return 0;
}
