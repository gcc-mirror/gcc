/* { dg-do run } */

signed char a, b;
int c;
short d;
void fn1(int p1)
{
  short e = 4;
  int f;
  d = 0;
  for (; d <= 0; d++)
    e = 0;
  if (e)
    goto L1;
L2:
  if (p1) {
      a = 9;
      for (; a; ++a) {
	  f = 5;
	  for (; f != 32; ++f)
	    c = 8;
L1:
	  if (b)
	    goto L2;
      }
  }
}

int main()
{
  fn1(1);
  return 0;
}
