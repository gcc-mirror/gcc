/* { dg-do compile } */
/* { dg-options "-O3" } */

int a[512];
int b;
void d()
{
  unsigned char c;
  for (; b; b++) {
      c = 1;
      for (; c; c <<= 1) {
	  a[b] <<= 8;
	  if (b & c)
	    a[b] = 1;
      }
  }
}
