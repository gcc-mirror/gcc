/* { dg-do compile } */
/* { dg-options "-O2" } */

// Hits *add_uxtdi_shift2 (*add_uxt<mode>_shift2).
/*
** add1:
** 	add	x0, x0, w1, uxtw 3
** 	ret
*/
unsigned long long *add1(unsigned long long *p, unsigned x)
{
  return p + x;
}

// Hits *add_zero_extendsi_di (*add_<optab><ALLX:mode>_<GPI:mode>).
/*
** add2:
** 	add	x0, x0, w1, uxtw
** 	ret
*/
unsigned long long add2(unsigned long long x, unsigned y)
{
  /* { dg-final { scan-assembler-times "add\tx0, x0, w1, uxtw" 1 { target ilp32 } } } */
  return x + y;
}

// Hits *add_extendsi_shft_di (*add_<optab><ALLX:mode>_shft_<GPI:mode>).
/*
** add3:
** 	add	x0, x0, w1, sxtw 3
** 	ret
*/
double *add3(double *p, int x)
{
  return p + x;
}

// add1 and add3 should both generate this on ILP32:
/* { dg-final { scan-assembler-times "add\tw0, w0, w1, lsl 3" 2 { target ilp32 } } } */

// Hits *sub_zero_extendsi_di (*sub_<optab><ALLX:mode>_<GPI:mode>).
/*
** sub1:
** 	sub	x0, x0, w1, uxtw
** 	ret
*/
unsigned long long sub1(unsigned long long x, unsigned n)
{
    /* { dg-final { scan-assembler-times "sub\tx0, x0, w1, uxtw" 1 { target ilp32 } } } */
    return x - n;
}

// Hits *sub_uxtdi_shift2 (*sub_uxt<mode>_shift2).
/*
** sub2:
** 	sub	x0, x0, w1, uxtw 3
** 	ret
*/
double *sub2(double *x, unsigned n)
{
  return x - n;
}

// Hits *sub_extendsi_shft_di (*sub_<optab><ALLX:mode>_shft_<GPI:mode>).
/*
** sub3:
** 	sub	x0, x0, w1, sxtw 3
** 	ret
*/
double *sub3(double *p, int n)
{
  return p - n;
}

// sub2 and sub3 should both generate this on ILP32:
/* { dg-final { scan-assembler-times "sub\tw0, w0, w1, lsl 3" 2 { target ilp32 } } } */

// Hits *adds_zero_extendsi_di (*adds_<optab><ALLX:mode>_<GPI:mode>).
int adds1(unsigned long long x, unsigned y)
{
  /* { dg-final { scan-assembler-times "adds\tx\[0-9\]+, x\[0-9\]+, w\[0-9\]+, uxtw" 1 } } */
  unsigned long long l = x + y;
  return !!l;
}

// Hits *adds_extendsi_shift_di (*adds_<optab><ALLX:mode>_shift_<GPI:mode>).
int adds2(long long x, int y)
{
  /* { dg-final { scan-assembler-times "adds\tx\[0-9\]+, x\[0-9\]+, w\[0-9\]+, sxtw 3" 1 } } */
  long long t = x + ((long long)y << 3);
  return !!t;
}

// Hits *subs_zero_extendsi_di (*subs_<optab><ALLX:mode>_<GPI:mode>).
unsigned long long z;
int subs1(unsigned long long x, unsigned y)
{
  /* { dg-final { scan-assembler-times "subs\tx\[0-9\]+, x\[0-9\]+, w\[0-9\]+, uxtw" 1 } } */
  unsigned long long t = x - y;
  z = t;
  return !!t;
}

// Hits *subs_extendsi_shift_di (*subs_<optab><ALLX:mode>_shift_<GPI:mode>).
unsigned long long *w;
int subs2(unsigned long long *x, int y)
{
  /* { dg-final { scan-assembler-times "subs\tx\[0-9\]+, x\[0-9\]+, w\[0-9\]+, sxtw 3" 1 { target lp64 } } } */
  /* { dg-final { scan-assembler-times "subs\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+, lsl 3" 1 { target ilp32 } } } */
  unsigned long long *t = x - y;
  w = t;
  return !!t;
}

// Hits *cmp_swp_zero_extendsi_regdi (*cmp_swp_<optab><ALLX:mode>_reg<GPI:mode>).
int cmp(unsigned long long x, unsigned y)
{
  /* { dg-final { scan-assembler-times "cmp\tx\[0-9\]+, w\[0-9\]+, uxtw" 1 } } */
  return !!(x - y);
}

// Hits *cmp_swp_extendsi_shft_di (*cmp_swp_<optab><ALLX:mode>_shft_<GPI:mode>).
int cmp2(unsigned long long x, int y)
{
  /* { dg-final { scan-assembler-times "cmp\tx\[0-9\]+, w\[0-9\]+, sxtw 3" 1 } } */
  return x == ((unsigned long long)y << 3);
}

/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */
