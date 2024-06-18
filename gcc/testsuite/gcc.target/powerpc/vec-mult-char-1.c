/* { dg-do run { target { powerpc*-*-* && vmx_hw } } } */
/* { dg-options "-maltivec" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

extern void abort (void);

vector unsigned char vmului(vector unsigned char v,
			    vector unsigned char i)
{
	return v * i;
}

vector signed char vmulsi(vector signed char v,
			  vector signed char i)
{
	return v * i;
}

int main ()
{
  vector unsigned char a = {2, 4, 6, 8, 10, 12, 14, 16,
			    18, 20, 22, 24, 26, 28, 30, 32};
  vector unsigned char b = {3, 6, 9, 12, 15, 18, 21, 24,
			    27, 30, 33, 36, 39, 42, 45, 48};
  vector unsigned char c = vmului (a, b);
  vector unsigned char expect_c = {6, 24, 54, 96, 150, 216, 38, 128,
				   230, 88, 214, 96, 246, 152, 70, 0};

  vector signed char d = {2, -4, 6, -8, 10, -12, 14, -16,
			  18, -20, 22, -24, 26, -28, 30, -32};
  vector signed char e = {3, 6, -9, -12, 15, 18, -21, -24,
			  27, 30, -33, -36, 39, 42, -45, -48};
  vector signed char f = vmulsi (d, e);
  vector signed char expect_f = {6, -24, -54, 96, -106, 40, -38, -128,
				 -26, -88, 42, 96, -10, 104, -70, 0};

  vector signed char g = {127, -128, 126, -126, 125, -125,  124, -124,
			  123, -123, 122, -122, 121, -121,  120, -120};
  vector signed char h = {  2,    2,  -2,   -2, 127,  127, -128, -128,
			    10,  10, -10,  -10,  64,   65,  -64,  -65};
  vector signed char i = vmulsi (g, h);
  vector signed char expect_i = {-2, 0, 4, -4, 3, -3, 0, 0,
				 -50, 50, 60, -60, 64, 71, 0, 120};

  if (!vec_all_eq (c, expect_c))
    abort ();
  if (!vec_all_eq (f, expect_f))
    abort ();
  if (!vec_all_eq (i, expect_i))
    abort ();
}
