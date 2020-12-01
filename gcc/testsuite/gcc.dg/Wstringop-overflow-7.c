/* Test to verify that --param ssa_name_def_chain_limit can be used to
   limit the maximum number of SSA_NAME assignments the warning follows.
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-array-bounds --param ssa-name-def-chain-limit=5" }  */

#define NOIPA __attribute__ ((noipa))

void* memset (void*, int, __SIZE_TYPE__);

char a9[9];

void sink (const char*, ...);

NOIPA void g2 (int i)
{
  if (i < 1) i = 1;

  char *p0 = a9;
  char *p1 = p0 + i;
  char *p2 = p1 + i;

  sink (p0, p1, p2);

  memset (p2, 0, 8);          // { dg-warning "\\\[-Wstringop-overflow" }
}

NOIPA void g3 (int i)
{
  if (i < 1) i = 1;

  char *p0 = a9;
  char *p1 = p0 + i;
  char *p2 = p1 + i;
  char *p3 = p2 + i;

  sink (p0, p1, p2, p3);

  memset (p3, 0, 7);          // { dg-warning "\\\[-Wstringop-overflow" }
}

NOIPA void g4 (int i)
{
  if (i < 1) i = 1;

  char *p0 = a9;
  char *p1 = p0 + i;
  char *p2 = p1 + i;
  char *p3 = p2 + i;
  char *p4 = p3 + i;

  sink (p0, p1, p2, p3, p4);

  memset (p4, 0, 6);          // { dg-warning "\\\[-Wstringop-overflow" }
}

NOIPA void g5 (int i)
{
  if (i < 1) i = 1;

  char *p0 = a9;
  char *p1 = p0 + i;
  char *p2 = p1 + i;
  char *p3 = p2 + i;
  char *p4 = p3 + i;
  char *p5 = p4 + i;

  sink (p0, p1, p2, p3, p4, p5);

  memset (p5, 0, 5);          // { dg-warning "\\\[-Wstringop-overflow" }
}

NOIPA void g6 (int i)
{
  if (i < 1) i = 1;

  char *p0 = a9;
  char *p1 = p0 + i;
  char *p2 = p1 + i;
  char *p3 = p2 + i;
  char *p4 = p3 + i;
  char *p5 = p4 + i;
  char *p6 = p5 + i;

  sink (p0, p1, p2, p3, p4, p5, p6);

  memset (p6, 0, 4);
}

NOIPA void g7 (int i)
{
  if (i < 1) i = 1;

  char *p0 = a9;
  char *p1 = p0 + i;
  char *p2 = p1 + i;
  char *p3 = p2 + i;
  char *p4 = p3 + i;
  char *p5 = p4 + i;
  char *p6 = p5 + i;
  char *p7 = p6 + i;

  sink (p0, p1, p2, p3, p4, p5, p6, p7);

  memset (p7, 0, 4);
}

NOIPA void g8 (int i)
{
  if (i < 1) i = 1;

  char *p0 = a9;
  char *p1 = p0 + i;
  char *p2 = p1 + i;
  char *p3 = p2 + i;
  char *p4 = p3 + i;
  char *p5 = p4 + i;
  char *p6 = p5 + i;
  char *p7 = p6 + i;
  char *p8 = p7 + i;

  sink (p0, p1, p2, p3, p4, p5, p6, p7, p8);

  memset (p8, 0, 2);
}
