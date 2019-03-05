/* { dg-do compile } */
/* { dg-options "-O3" } */

int b;
extern unsigned c[];
unsigned d;
long e;

void f()
{
  unsigned g, h;
  for (; d; d += 2) {
      g = 1;
      for (; g; g += 3) {
	  h = 2;
	  for (; h < 6; h++)
	    c[h] = c[h] - b - ~e;
      }
  }
}
