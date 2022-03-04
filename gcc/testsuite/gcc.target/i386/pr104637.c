/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-Og -fno-forward-propagate -mavx -Wno-div-by-zero" } */

typedef short __attribute__((__vector_size__ (64))) U;
typedef unsigned long long __attribute__((__vector_size__ (32))) V;
typedef long double __attribute__((__vector_size__ (64))) F;

int i;
U u;
F f;

void
foo (char a, char b, _Complex char c, V v)
{
  u = (U) { u[0] / 0, u[1] / 0, u[2] / 0, u[3] / 0, u[4] / 0, u[5] / 0, u[6] / 0, u[7] / 0,
	    u[8] / 0, u[9] / 0, u[10] / 0, u[11] / 0, u[12] / 0, u[13] / 0, u[14] / 0, u[15] / 0,
	    u[16] / 0, u[17] / 0, u[18] / 0, u[19] / 0, u[20] / 0, u[21] / 0, u[22] / 0, u[23] / 0,
	    u[24] / 0, u[25] / 0, u[26] / 0, u[27] / 0, u[28] / 0, u[29] / 0, u[30] / 0, u[31] / 0 };
  c += i;
  f = (F) { v[0], v[1], v[2], v[3] };
  i = (char) (__imag__ c + i);
}
