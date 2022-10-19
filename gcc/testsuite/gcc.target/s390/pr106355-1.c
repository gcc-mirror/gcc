/* { dg-do compile } */
/* { dg-options "-foptimize-sibling-calls" } */
/* { dg-final { scan-assembler {brasl\t%r\d+,bar4} } } */
/* { dg-final { scan-assembler {brasl\t%r\d+,bar8} } } */

/* Parameter E is passed in GPR 6 which is call-saved which prohibits
   sibling call optimization.  This must hold true also if the mode of the
   parameter is BLKmode.  */

/* 4 byte */

typedef struct
{
  char x;
  char y[3];
} t4;

extern t4 e4;

extern void bar4 (int a, int b, int c, int d, t4 e4);

void foo4 (int a, int b, int c, int d)
{
  bar4 (a, b, c, d, e4);
}

/* 8 byte */

typedef struct
{
  short x;
  char y[6];
} t8;

extern t8 e8;

extern void bar8 (int a, int b, int c, int d, t8 e8);

void foo8 (int a, int b, int c, int d)
{
  bar8 (a, b, c, d, e8);
}
