/* { dg-do assemble { target { apxf && { ! ia32 } } } } */
/* { dg-options "-mapxf -O3 -w" } */

long a;
int b, d, e;
void
g (void)
{
  int c;
  _Bool f;
  __asm__("" : "=c"(c));
  switch (d)
    case 2:
      e = f = c & 2;
      if (f)
	a = b;
}
