/* { dg-do compile } */

int f();
void g();
int a, b, c, e;
int d[10];
int main()
{
  if (c)
    if (f())
      {
	g();
	if (e) {
	    a = 0;
	    for (; a != 6; a = a + 2)
	      {
		b = 0;
		for (; b <= 3; b++)
		  d[b] &= 1;
	      }
	}
      }
  return 0;
}
