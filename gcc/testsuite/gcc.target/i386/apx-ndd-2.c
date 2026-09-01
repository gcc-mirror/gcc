/* { dg-do assemble { target { apxf && { { ! ia32 } && { ! *-*-darwin* } } } } } */
/* { dg-options "-mapxf -mtune-ctrl=enable_ndd_mem -O3 -w" } */

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
