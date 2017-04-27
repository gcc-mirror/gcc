/* { dg-do compile } */

signed char a, b;
void fn1()
{
  signed char c, e;
  short d;
  if (0) {
      for (; d;) {
l1:
	  for (c = 7; a; c++)
	    ;
	  e = 6;
	  for (; b; e++)
	    ;
      }
      c -= e;
  }
  if (d == 7)
    goto l1;
  a = c;
}
