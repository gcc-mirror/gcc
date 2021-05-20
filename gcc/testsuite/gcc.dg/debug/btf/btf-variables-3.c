/* Test BTF generation for static versus global variables.

   BTF_KIND_VAR types represeting variables are followed by a 32-bit
   "linkage", which can take one of currently two valid values:
      0 = static
      1 = global

   In this test, make a few static and a few global variables, and ensure
   they are noted with the correct "linkage" values.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

/* Expect 6 variables.  */
/* { dg-final { scan-assembler-times "\[\t \]0xe000000\[\t \]+\[^\n\]*btv_info" 6 } } */

/* 3 global, 3 static.  */
/* { dg-final { scan-assembler-times "\[\t \]0\[\t \]+\[^\n\]*btv_linkage" 3 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x1\[\t \]+\[^\n\]*btv_linkage" 3 } } */

int a;

static long b;

struct foo {
  int x;
  int y;
};

struct foo g_foo;

static struct foo s_foo;

static unsigned int s_arr [10][5];

unsigned int g_arr [20];
