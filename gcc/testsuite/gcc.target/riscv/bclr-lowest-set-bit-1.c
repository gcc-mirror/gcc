/* { dg-do compile } */
/* { dg-options "-march=rv64gcb -mabi=lp64d" { target { rv64} } } */
/* { dg-options "-march=rv32gcb -mabi=ilp32" { target { rv32} } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

int foo1_res1;
int foo1_res2;
void foo1(unsigned int x)
{
  unsigned int tz = __builtin_ctz (x);
  unsigned int t = x - 1UL;
  foo1_res1 = t & x;
  foo1_res2 = tz;
}

int foo2_res1;
int foo2_res2;
void foo2(unsigned int x)
{
  unsigned int t = x - 1UL;
  unsigned int tz = __builtin_ctz (x);
  foo2_res1 = t & x;
  foo2_res2 = tz;
}

int foo3_res1;
int foo3_res2;
void foo3(unsigned int x)
{
  unsigned int t = x - 1UL;
  foo3_res1 = t & x;
  unsigned int tz = __builtin_ctz (x);
  foo3_res2 = tz;
}

unsigned int foo4_res1;
unsigned int foo4_res2;
void foo4(unsigned int x)
{
  unsigned int tz = __builtin_ctz (x);
  unsigned int t = x - 1UL;
  foo4_res1 = t & x;
  foo4_res2 = tz;
}

unsigned int foo5_res1;
unsigned int foo5_res2;
void foo5(unsigned int x)
{
  unsigned int t = x - 1UL;
  unsigned int tz = __builtin_ctz (x);
  foo5_res1 = t & x;
  foo5_res2 = tz;
}

unsigned int foo6_res1;
unsigned int foo6_res2;
void foo6(unsigned int x)
{
  unsigned int t = x - 1UL;
  foo6_res1 = t & x;
  unsigned int tz = __builtin_ctzl (x);
  foo6_res2 = tz;
}

long foo7_res1;
long foo7_res2;
void foo7(unsigned long x)
{
  unsigned long tz = __builtin_ctzl (x);
  unsigned long t = x - 1UL;
  foo7_res1 = t & x;
  foo7_res2 = tz;
}

long foo8_res1;
long foo8_res2;
void foo8(unsigned long x)
{
  unsigned long t = x - 1UL;
  unsigned long tz = __builtin_ctzl (x);
  foo8_res1 = t & x;
  foo8_res2 = tz;
}

long foo9_res1;
long foo9_res2;
void foo9(unsigned long x)
{
  unsigned long t = x - 1UL;
  foo9_res1 = t & x;
  unsigned long tz = __builtin_ctzl (x);
  foo9_res2 = tz;
}

unsigned long foo10_res1;
unsigned long foo10_res2;
void foo10(unsigned long x)
{
  unsigned long tz = __builtin_ctzl (x);
  unsigned long t = x - 1UL;
  foo10_res1 = t & x;
  foo10_res2 = tz;
}

unsigned long foo11_res1;
unsigned long foo11_res2;
void foo11(unsigned long x)
{
  unsigned long t = x - 1UL;
  unsigned long tz = __builtin_ctzl (x);
  foo11_res1 = t & x;
  foo11_res2 = tz;
}

unsigned long foo12_res1;
unsigned long foo12_res2;
void foo12(unsigned long x)
{
  unsigned long t = x - 1UL;
  foo12_res1 = t & x;
  unsigned long tz = __builtin_ctzl (x);
  foo12_res2 = tz;
}

/* { dg-final { scan-assembler-not "\\sand\\s" } } */
/* { dg-final { scan-assembler-times "\\sbclr\\s" 12 } } */

