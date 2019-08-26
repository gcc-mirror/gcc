/* Test to verify that --param ssa_name_def_chain_limit can be used to
   limit the maximum number of SSA_NAME assignments the built-in code
   follows to determine the variable value/string length.
   { dg-do compile }
   { dg-options "-O2 -Wall --param ssa-name-def-chain-limit=4 -fdump-tree-optimized" } */

void abort (void);
int sprintf (char * restrict, const char *restrict, ...);

void sink (const char*, ...);

const char a0[] = "";
const char a1[] = "1";
const char a2[] = "12";
const char a3[] = "123";
const char a4[] = "1234";
const char a5[] = "12345";
const char a6[] = "123456";
const char a7[] = "1234567";
const char a8[] = "12345678";
const char a9[] = "123456789";

int i0, i1, i2, i3, i4, i5, i6, i7, i8;

void g1 (char *d)
{
  const char *p0 = i0 ? a0 : a1;
  const char *p1 = i1 ? p0 : a2;

  sink (p0, p1);

  if (sprintf (d, "%s", p1) > 2)
    abort ();
}

void g2 (char *d)
{
  const char *p0 = i0 ? a0 : a1;
  const char *p1 = i1 ? p0 : a2;
  const char *p2 = i2 ? p1 : a3;

  sink (p0, p1, p2);

  if (sprintf (d, "%s", p2) > 3)
    abort ();
}

void g3 (char *d)
{
  const char *p0 = i0 ? a0 : a1;
  const char *p1 = i1 ? p0 : a2;
  const char *p2 = i2 ? p1 : a3;
  const char *p3 = i3 ? p2 : a4;

  sink (p0, p1, p2, p3);

  if (sprintf (d, "%s", p3) > 4)
    abort ();
}

void g4 (char *d)
{
  const char *p0 = i0 ? a0 : a1;
  const char *p1 = i1 ? p0 : a2;
  const char *p2 = i2 ? p1 : a3;
  const char *p3 = i3 ? p2 : a4;
  const char *p4 = i4 ? p3 : a5;

  sink (p0, p1, p2, p3, p4);

  // p4 below is the result of the following five PHI assignments
  // and with the limit set to 4 the sprintf call result is not
  // determined:
  //   iftmp.0_7 = PHI <&a0(2), &a1(3)>
  //   iftmp.2_8 = PHI <iftmp.0_7(4), &a2(5)>
  //   iftmp.4_9 = PHI <iftmp.2_8(6), &a3(7)>
  //   iftmp.6_10 = PHI <iftmp.4_9(8), &a4(9)>
  //   iftmp.8_17 = PHI <iftmp.6_10(10), &a5(11)>
  //   p4 = iftmp.8_17
  extern void keep_g4 (void);
  if (sprintf (d, "%s", p4) > 5)
    keep_g4 ();
}

void g5 (char *d)
{
  const char *p0 = i0 ? a0 : a1;
  const char *p1 = i1 ? p0 : a2;
  const char *p2 = i2 ? p1 : a3;
  const char *p3 = i3 ? p2 : a4;
  const char *p4 = i4 ? p3 : a5;
  const char *p5 = i5 ? p4 : a6;

  sink (p0, p1, p2, p3, p4, p5);

  extern void keep_g5 (void);
  if (sprintf (d, "%s", p5) > 6)
    keep_g5 ();

  /* { dg-final { scan-tree-dump-times "keep_g5" 1 "optimized" } } */
}

void g6 (char *d)
{
  const char *p0 = i0 ? a0 : a1;
  const char *p1 = i1 ? p0 : a2;
  const char *p2 = i2 ? p1 : a3;
  const char *p3 = i3 ? p2 : a4;
  const char *p4 = i4 ? p3 : a5;
  const char *p5 = i5 ? p4 : a6;
  const char *p6 = i6 ? p5 : a7;

  sink (p0, p1, p2, p3, p4, p5, p6);

  extern void keep_g6 (void);
  if (sprintf (d, "%s", p6) > 7)
    keep_g6 ();

  /* { dg-final { scan-tree-dump-times "keep_g6" 1 "optimized" } } */
}

void g7 (char *d)
{
  const char *p0 = i0 ? a0 : a1;
  const char *p1 = i1 ? p0 : a2;
  const char *p2 = i2 ? p1 : a3;
  const char *p3 = i3 ? p2 : a4;
  const char *p4 = i4 ? p3 : a5;
  const char *p5 = i5 ? p4 : a6;
  const char *p6 = i6 ? p5 : a7;
  const char *p7 = i7 ? p6 : a8;

  sink (p0, p1, p2, p3, p4, p5, p6, p7);

  extern void keep_g7 (void);
  if (sprintf (d, "%s", p7) > 8)
    keep_g7 ();

  /* { dg-final { scan-tree-dump-times "keep_g7" 1 "optimized" } } */
}

void g8 (char *d)
{
  const char *p0 = i0 ? a0 : a1;
  const char *p1 = i1 ? p0 : a2;
  const char *p2 = i2 ? p1 : a3;
  const char *p3 = i3 ? p2 : a4;
  const char *p4 = i4 ? p3 : a5;
  const char *p5 = i5 ? p4 : a6;
  const char *p6 = i6 ? p5 : a7;
  const char *p7 = i7 ? p6 : a8;
  const char *p8 = i8 ? p7 : a9;

  sink (p0, p1, p2, p3, p4, p5, p6, p7, p8);

  extern void keep_g8 (void);
  if (sprintf (d, "%s", p8) > 9)
    keep_g8 ();

  /* { dg-final { scan-tree-dump-times "keep_g8" 1 "optimized" } } */
}

/* { dg-final { scan-tree-dump-not "abort" "optimized" } } */
