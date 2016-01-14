/* { dg-do compile } */

void fn2 ();

int a, b, c;

void fn1()
{
  for (;;) {
      int *d;
      fn2();
      c = 0;
      for (; c <= 3; c++) {
	  *d ^= 9;
	  b = 0;
	  for (; b <= 3; b++)
	    *d ^= a;
      }
  }
}
