/* Test to verify that --param ssa_name_def_chain_limit can be used to
   limit the maximum number of SSA_NAME assignments the warning follows.
   { dg-do compile }
   { dg-options "-O2 -Wall --param ssa-name-def-chain-limit=4" }  */

#define NOIPA __attribute__ ((noipa))

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

void f (const char*, ...);

int i0, i1, i2, i3, i4, i5, i6, i7, i8;

NOIPA int g2 (int i)
{
  if (i < 1) i = 1;

  const char *p0 = a9;
  const char *p1 = p0 + i;
  const char *p2 = p1 + i;

  f (p0, p1, p2);

  return p2[8];     // { dg-warning "\\\[-Warray-bounds]" }
}

NOIPA int g3 (int i)
{
  if (i < 1) i = 1;

  const char *p0 = a9;
  const char *p1 = p0 + i;
  const char *p2 = p1 + i;
  const char *p3 = p2 + i;

  f (p0, p1, p2, p3);

  return p3[7];     // { dg-warning "\\\[-Warray-bounds]" }
}

NOIPA int g4 (int i)
{
  if (i < 1) i = 1;

  const char *p0 = a9;
  const char *p1 = p0 + i;
  const char *p2 = p1 + i;
  const char *p3 = p2 + i;
  const char *p4 = p3 + i;

  f (p0, p1, p2, p3, p4);

  return p4[6];     // { dg-warning "\\\[-Warray-bounds]" }
}

NOIPA int g5 (int i)
{
  if (i < 1) i = 1;

  const char *p0 = a9;
  const char *p1 = p0 + i;
  const char *p2 = p1 + i;
  const char *p3 = p2 + i;
  const char *p4 = p3 + i;
  const char *p5 = p4 + i;

  f (p0, p1, p2, p3, p4, p5);

  return p5[5];
}

NOIPA int g6 (int i)
{
  if (i < 1) i = 1;

  const char *p0 = a9;
  const char *p1 = p0 + i;
  const char *p2 = p1 + i;
  const char *p3 = p2 + i;
  const char *p4 = p3 + i;
  const char *p5 = p4 + i;
  const char *p6 = p5 + i;

  f (p0, p1, p2, p3, p4, p5, p6);

  return p6[4];
}

NOIPA int g7 (int i)
{
  if (i < 1) i = 1;

  const char *p0 = a9;
  const char *p1 = p0 + i;
  const char *p2 = p1 + i;
  const char *p3 = p2 + i;
  const char *p4 = p3 + i;
  const char *p5 = p4 + i;
  const char *p6 = p5 + i;
  const char *p7 = p6 + i;

  f (p0, p1, p2, p3, p4, p5, p6, p7);

  return p7[3];
}

NOIPA int g8 (int i)
{
  if (i < 1) i = 1;

  const char *p0 = a9;
  const char *p1 = p0 + i;
  const char *p2 = p1 + i;
  const char *p3 = p2 + i;
  const char *p4 = p3 + i;
  const char *p5 = p4 + i;
  const char *p6 = p5 + i;
  const char *p7 = p6 + i;
  const char *p8 = p7 + i;

  f (p0, p1, p2, p3, p4, p5, p6, p7, p8);

  return p8[2];
}
