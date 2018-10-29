/* PR target/69706 */
/* Reported by John Paul Adrian Glaubitz <glaubitz@physik.fu-berlin.de> */

/* { dg-do run } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-std=gnu99" } */

extern void abort (void);


/* Pass a 12-byte structure partially in slot #15 and on the stack.  */

struct t_rgb { float r, g, b; };

void write_xpm (void *out, unsigned int flags, const char *title, 
	        const char *legend, const char *label_x, const char *label_y,
	        int n_x, int n_y, float axis_x[], float axis_y[], float *mat[],
	        float lo, float hi, struct t_rgb rlo, struct t_rgb rhi)
{
  register float f30 asm ("f30");
  register float f31 asm ("f31");

  if (f30 != 1.0f)
    abort ();

  if (f31 != 2.0f)
    abort ();

  if (rhi.r != 1.0f)
    abort ();

  if (rhi.g != 2.0f)
    abort ();

  if (rhi.b != 3.0f)
    abort ();
}


/* Pass a 16-byte structure partially in slot #15 and on the stack.  */

struct S1 { _Complex float f1; _Complex float f2; };

void f1 (int p1, int p2, int p3, int p4, int p5, int p6, int p7, int p8,
	 int p9, int p10, int p11, int p12, int p13, int p14, int p15,
	 struct S1 s1)
{
  register float f30 asm ("f30");
  register float f31 asm ("f31");

  if (f30 != 4.0f)
    abort ();

  if (f31 != 5.0f)
    abort ();

  if (__real__ s1.f1 != 4.0f)
    abort ();

  if (__imag__ s1.f1 != 5.0f)
    abort ();

  if (__real__ s1.f2 != 6.0f)
    abort ();

  if (__imag__ s1.f2 != 7.0f)
    abort ();
}


/* Pass a 16-byte structure partially in slot #15 and on the stack.  */

struct S2 { double d1; double d2; };

void f2 (int p1, int p2, int p3, int p4, int p5, int p6, int p7, int p8,
	 int p9, int p10, int p11, int p12, int p13, int p14, int p15,
	 struct S2 s2)
{
  register double d30 asm ("f30");

  if (d30 != 1.0)
    abort ();

  if (s2.d1 != 1.0)
    abort ();

  if (s2.d2 != 2.0)
    abort ();
}


/* Pass a 16-byte structure partially in slot #15 and on the stack.  */

struct S3 { _Complex double d; };

void f3 (int p1, int p2, int p3, int p4, int p5, int p6, int p7, int p8,
	 int p9, int p10, int p11, int p12, int p13, int p14, int p15,
	 struct S3 s3)
{
  register double d30 asm ("f30");

  if (d30 != 3.0)
    abort ();

  if (__real__ s3.d != 3.0)
    abort ();

  if (__imag__ s3.d != 4.0)
    abort ();
}


/* Pass a 16-byte structure entirely on the stack.  */

struct S4 { long l; double d; };

void f4 (int p1, int p2, int p3, int p4, int p5, int p6, int p7, int p8,
	 int p9, int p10, int p11, int p12, int p13, int p14, int p15,
	 struct S4 s4)
{
  if (s4.l != 5)
    abort ();

  if (s4.d != 6.0)
    abort ();
}


#define PI 3.141592654

int main (void)
{
  struct t_rgb lo = { -1.0f, -2.0f, -3.0f };
  struct t_rgb hi = { 1.0f, 2.0f, 3.0f };
  float arrf[1];
  float *arrp[1];
  struct S1 s1 = { 4.0f + 5.0fi, 6.0f + 7.0fi };
  struct S2 s2 = { 1.0, 2.0 };
  struct S3 s3 = { 3.0 + 4.0i };
  struct S4 s4 = { 5, 6.0 };
  register double d32 asm ("f32") = PI;

  write_xpm (0, 0, "", "", "", "", 0, 0, arrf, arrf, arrp, 0.0f, 0.0f, lo, hi);

  f1 (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, s1);

  f2 (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, s2);

  f3 (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, s3);

  f4 (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, s4);

  if (d32 != PI)
    abort ();

  return 0;
}
