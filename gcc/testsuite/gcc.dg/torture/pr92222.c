/* { dg-do compile } */
/* { dg-additional-options "-ftree-vectorize" } */

unsigned char *a;
int b;
void f();
void c()
{
  char *d;
  int e;
  for (; b; b++) {
      e = 7;
      for (; e >= 0; e--)
	*d++ = a[b] & 1 << e ? '1' : '0';
  }
  f();
}
