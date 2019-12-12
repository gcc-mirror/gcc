/* Test to verify that --param ssa_name_def_chain_limit can be used to
   limit the maximum number of SSA_NAME assignments the built-in code
   follows.
   { dg-do compile }
   { dg-options "-O2 -Wall -Wformat-truncation=2 --param ssa-name-def-chain-limit=4 -fdump-tree-optimized" } */

typedef __SIZE_TYPE__ size_t;

int snprintf (char * restrict, size_t, const char *restrict, ...);

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

  snprintf (d, 1, "%s", p1);    // { dg-warning "\\\[-Wformat-truncation" }
}

void g2 (char *d)
{
  const char *p0 = i0 ? a0 : a1;
  const char *p1 = i1 ? p0 : a2;
  const char *p2 = i2 ? p1 : a3;

  sink (p0, p1, p2);

  snprintf (d, 2, "%s", p2);    // { dg-warning "\\\[-Wformat-truncation" }
}

void g3 (char *d)
{
  const char *p0 = i0 ? a0 : a1;
  const char *p1 = i1 ? p0 : a2;
  const char *p2 = i2 ? p1 : a3;
  const char *p3 = i3 ? p2 : a4;

  sink (p0, p1, p2, p3);

  snprintf (d, 3, "%s", p3);    // { dg-warning "\\\[-Wformat-truncation" }
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
  // and with the limit set to 4 the snprintf call is not diagnosed
  //   iftmp.0_7 = PHI <&a0(2), &a1(3)>
  //   iftmp.2_8 = PHI <iftmp.0_7(4), &a2(5)>
  //   iftmp.4_9 = PHI <iftmp.2_8(6), &a3(7)>
  //   iftmp.6_10 = PHI <iftmp.4_9(8), &a4(9)>
  //   iftmp.8_17 = PHI <iftmp.6_10(10), &a5(11)>
  //   p4 = iftmp.8_17
  snprintf (d, 4, "%s", p4);
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

  snprintf (d, 5, "%s", p5);
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

  snprintf (d, 6, "%s", p6);
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

  snprintf (d, 7, "%s", p7);
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

  snprintf (d, 8, "%s", p8);
}
