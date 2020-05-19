/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

typedef double c_t;
typedef int a_t;
int f(a_t a1, a_t a2) {
  return (c_t) a1 < (c_t) a2;
  /* { dg-final { scan-tree-dump "return (<retval> = )?a1 < a2" "original" } } */
}

void f1(short a, short b)
{
  volatile int s_s;
  s_s = (float) a < (float) b;
  /* { dg-final { scan-tree-dump "s_s = a < b" "original" } } */
}

void f2(unsigned short a, unsigned short b)
{
  volatile int us_us;
  us_us = (float) a < (float) b;
  /* { dg-final { scan-tree-dump "us_us = a < b" "original" } } */
}

/* We don't optimize here because neither of integral types is
   subset of the other.  */
void f3(unsigned short a, short b)
{
  volatile int us_s;
  us_s = (float) a < (float) b;
  /* { dg-final { scan-tree-dump "us_s = \\(float\\) a < \\(float\\) b" "original" } } */
}

void f4(unsigned short a, int b)
{
  volatile int us_i;
  us_i = (double) a < (double) b;
  /* { dg-final { scan-tree-dump "us_i = \\(int\\) a < b" "original" { target { ! short_eq_int } } } } */
}

void f4_short_eq_int(unsigned short a, long b)
{
  volatile long us_l;
  us_l = (double) a < (double) b;
  /* { dg-final { scan-tree-dump "us_l = \\(long int\\) a < b" "original" { target { short_eq_int } } } } */
}

/* We don't optimize here because neither of integral types is
   subset of the other.  */
void f5(short a, unsigned int b)
{
  volatile int s_ui;
  s_ui = (double) a < (double) b;
  /* { dg-final { scan-tree-dump "s_ui = \\(double\\) a < \\(double\\) b" "original" } } */
}
