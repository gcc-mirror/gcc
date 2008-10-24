/* { dg-do run } */
/* { dg-options "-O2" } */
/* Test that arguments are passed in the correct location according to the ABI.  */

#include <stdlib.h>

/* Hack to allow calling func_asm which takes 84 arguments that are scalars.
   The function func_call takes 84 union quadword arguments, so we can check to
   see if each scalar is passed in the correct location.  This asm glues the
   two functions together, so that the compiler is not aware of the
   aliasing.  */
__asm__ ("func_asm = func_call");

typedef unsigned int uqword __attribute__((mode(TI)));
typedef int qword __attribute__((mode(TI)));

union u
{
  uqword		uq;
  qword			sq;
  double		d[2];
  float			f[4];
  unsigned long long	ull[2];
  long long		sll[2];
  unsigned long		ul[4];
  long			sl[4];
  unsigned int		ui[4];
  int			si[4];
  unsigned short	us[8];
  short			ss[8];
  unsigned char		uc[16];
  signed char		sc[16];
};


extern void func_asm(signed char a1,
		     unsigned char a2,
		     short a3,
		     unsigned short a4,
		     int a5,
		     unsigned int a6,
		     long a7,
		     unsigned long a8,
		     long long a9,
		     unsigned long long a10,
		     float a11,
		     double a12,
		     int a13,
		     int a14,
		     int a15,
		     int a16,
		     int a17,
		     int a18,
		     int a19,
		     int a20,
		     int a21,
		     int a22,
		     int a23,
		     int a24,
		     int a25,
		     int a26,
		     int a27,
		     int a28,
		     int a29,
		     int a30,
		     int a31,
		     int a32,
		     int a33,
		     int a34,
		     int a35,
		     int a36,
		     int a37,
		     int a38,
		     int a39,
		     int a40,
		     int a41,
		     int a42,
		     int a43,
		     int a44,
		     int a45,
		     int a46,
		     int a47,
		     int a48,
		     int a49,
		     int a50,
		     int a51,
		     int a52,
		     int a53,
		     int a54,
		     int a55,
		     int a56,
		     int a57,
		     int a58,
		     int a59,
		     int a60,
		     int a61,
		     int a62,
		     int a63,
		     int a64,
		     int a65,
		     int a66,
		     int a67,
		     int a68,
		     int a69,
		     int a70,
		     int a71,
		     int a72,
		     signed char a73,
		     unsigned char a74,
		     short a75,
		     unsigned short a76,
		     int a77,
		     unsigned int a78,
		     long a79,
		     unsigned long a80,
		     long long a81,
		     unsigned long long a82,
		     float a83,
		     double a84);

void func_call(union u a1,
	       union u a2,
	       union u a3,
	       union u a4,
	       union u a5,
	       union u a6,
	       union u a7,
	       union u a8,
	       union u a9,
	       union u a10,
	       union u a11,
	       union u a12,
	       union u a13,
	       union u a14,
	       union u a15,
	       union u a16,
	       union u a17,
	       union u a18,
	       union u a19,
	       union u a20,
	       union u a21,
	       union u a22,
	       union u a23,
	       union u a24,
	       union u a25,
	       union u a26,
	       union u a27,
	       union u a28,
	       union u a29,
	       union u a30,
	       union u a31,
	       union u a32,
	       union u a33,
	       union u a34,
	       union u a35,
	       union u a36,
	       union u a37,
	       union u a38,
	       union u a39,
	       union u a40,
	       union u a41,
	       union u a42,
	       union u a43,
	       union u a44,
	       union u a45,
	       union u a46,
	       union u a47,
	       union u a48,
	       union u a49,
	       union u a50,
	       union u a51,
	       union u a52,
	       union u a53,
	       union u a54,
	       union u a55,
	       union u a56,
	       union u a57,
	       union u a58,
	       union u a59,
	       union u a60,
	       union u a61,
	       union u a62,
	       union u a63,
	       union u a64,
	       union u a65,
	       union u a66,
	       union u a67,
	       union u a68,
	       union u a69,
	       union u a70,
	       union u a71,
	       union u a72,
	       union u a73,
	       union u a74,
	       union u a75,
	       union u a76,
	       union u a77,
	       union u a78,
	       union u a79,
	       union u a80,
	       union u a81,
	       union u a82,
	       union u a83,
	       union u a84)
{
  /* arguments passed in registers */
  if (a1.sc[3] != -1)			/* signed char */
    abort ();

  if (a2.uc[3] != +2)			/* unsigned char */
    abort ();

  if (a3.ss[1] != -3)			/* short */
    abort ();

  if (a4.us[1] != +4)			/* unsigned short */
    abort ();

  if (a5.si[0] != -5)			/* int */
    abort ();

  if (a6.ui[0] != +6)			/* unsigned int */
    abort ();

  if (a7.sl[0] != -7)			/* long */
    abort ();

  if (a8.ul[0] != +8)			/* unsigned long */
    abort ();

  if (a9.sll[0] != -9)			/* long long */
    abort ();

  if (a10.ull[0] != +10)		/* unsigned long long */
    abort ();

  if (a11.f[0] != -11.0f)		/* float */
    abort ();

  if (a12.d[0] != +12.0)		/* double */
    abort ();

  if (a13.si[0] != -13)			/* int */
    abort ();

  if (a14.si[0] != +14)			/* int */
    abort ();

  if (a15.si[0] != -15)			/* int */
    abort ();

  if (a16.si[0] != +16)			/* int */
    abort ();

  if (a17.si[0] != -17)			/* int */
    abort ();

  if (a18.si[0] != +18)			/* int */
    abort ();

  if (a19.si[0] != -19)			/* int */
    abort ();

  if (a20.si[0] != +20)			/* int */
    abort ();

  if (a21.si[0] != -21)			/* int */
    abort ();

  if (a22.si[0] != +22)			/* int */
    abort ();

  if (a23.si[0] != -23)			/* int */
    abort ();

  if (a24.si[0] != +24)			/* int */
    abort ();

  if (a25.si[0] != -25)			/* int */
    abort ();

  if (a26.si[0] != +26)			/* int */
    abort ();

  if (a27.si[0] != -27)			/* int */
    abort ();

  if (a28.si[0] != +28)			/* int */
    abort ();

  if (a29.si[0] != -29)			/* int */
    abort ();

  if (a30.si[0] != +30)			/* int */
    abort ();

  if (a31.si[0] != -31)			/* int */
    abort ();

  if (a32.si[0] != +32)			/* int */
    abort ();

  if (a33.si[0] != -33)			/* int */
    abort ();

  if (a34.si[0] != +34)			/* int */
    abort ();

  if (a35.si[0] != -35)			/* int */
    abort ();

  if (a36.si[0] != +36)			/* int */
    abort ();

  if (a37.si[0] != -37)			/* int */
    abort ();

  if (a38.si[0] != +38)			/* int */
    abort ();

  if (a39.si[0] != -39)			/* int */
    abort ();

  if (a40.si[0] != +40)			/* int */
    abort ();

  if (a41.si[0] != -41)			/* int */
    abort ();

  if (a42.si[0] != +42)			/* int */
    abort ();

  if (a43.si[0] != -43)			/* int */
    abort ();

  if (a44.si[0] != +44)			/* int */
    abort ();

  if (a45.si[0] != -45)			/* int */
    abort ();

  if (a46.si[0] != +46)			/* int */
    abort ();

  if (a47.si[0] != -47)			/* int */
    abort ();

  if (a48.si[0] != +48)			/* int */
    abort ();

  if (a49.si[0] != -49)			/* int */
    abort ();

  if (a50.si[0] != +50)			/* int */
    abort ();

  if (a51.si[0] != -51)			/* int */
    abort ();

  if (a52.si[0] != +52)			/* int */
    abort ();

  if (a53.si[0] != -53)			/* int */
    abort ();

  if (a54.si[0] != +54)			/* int */
    abort ();

  if (a55.si[0] != -55)			/* int */
    abort ();

  if (a56.si[0] != +56)			/* int */
    abort ();

  if (a57.si[0] != -57)			/* int */
    abort ();

  if (a58.si[0] != +58)			/* int */
    abort ();

  if (a59.si[0] != -59)			/* int */
    abort ();

  if (a60.si[0] != +60)			/* int */
    abort ();

  if (a61.si[0] != -61)			/* int */
    abort ();

  if (a62.si[0] != +62)			/* int */
    abort ();

  if (a63.si[0] != -63)			/* int */
    abort ();

  if (a64.si[0] != +64)			/* int */
    abort ();

  if (a65.si[0] != -65)			/* int */
    abort ();

  if (a66.si[0] != +66)			/* int */
    abort ();

  if (a67.si[0] != -67)			/* int */
    abort ();

  if (a68.si[0] != +68)			/* int */
    abort ();

  if (a69.si[0] != -69)			/* int */
    abort ();

  if (a70.si[0] != +70)			/* int */
    abort ();

  if (a71.si[0] != -71)			/* int */
    abort ();

  if (a72.si[0] != +72)			/* int */
    abort ();

  /* arguments passed on the stack */
  if (a73.sc[3] != -73)			/* signed char */
    abort ();

  if (a74.uc[3] != 74)			/* unsigned char */
    abort ();

  if (a75.ss[1] != -75)			/* short */
    abort ();

  if (a76.us[1] != +76)			/* unsigned short */
    abort ();

  if (a77.si[0] != -77)			/* int */
    abort ();

  if (a78.ui[0] != +78)			/* unsigned int */
    abort ();

  if (a79.sl[0] != -79)			/* long */
    abort ();

  if (a80.ul[0] != +80)			/* unsigned long */
    abort ();

  if (a81.sll[0] != -81)		/* long long */
    abort ();

  if (a82.ull[0] != +82)		/* unsigned long long */
    abort ();

  if (a83.f[0] != -83.0f)		/* float */
    abort ();

  if (a84.d[0] != +84.0)		/* double */
    abort ();
}

int main(void)
{
  func_asm(-1,   +2,  -3,  +4,  -5,  +6,  -7,  +8,  -9, +10,
	   -11, +12, -13, +14, -15, +16, -17, +18, -19, +20,
	   -21, +22, -23, +24, -25, +26, -27, +28, -29, +30,
	   -31, +32, -33, +34, -35, +36, -37, +38, -39, +40,
	   -41, +42, -43, +44, -45, +46, -47, +48, -49, +50,
	   -51, +52, -53, +54, -55, +56, -57, +58, -59, +60,
	   -61, +62, -63, +64, -65, +66, -67, +68, -69, +70,
	   -71, +72, -73, +74, -75, +76, -77, +78, -79, +80,
	   -81, +82, -83, +84);

  return 0;
}
