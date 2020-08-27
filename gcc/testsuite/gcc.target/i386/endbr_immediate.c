/* PR target/96350 */
/* { dg-do compile } */
/* { dg-options "-fcf-protection -O2" } */
/* { dg-final { scan-assembler-not "$-81915917" { target { ia32 } } } } */
/* { dg-final { scan-assembler-not "$-98693133" { target { ! ia32 } } } } *
/* { dg-final { scan-assembler-not "$-423883778574778368" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "\[ \t\]*-81915917" { target { ia32 } } } } */
/* { dg-final { scan-assembler "\[ \t\]*-98693133" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "\[ \t\]*-423883778574778368" { target { ! ia32 } } } } */


#ifdef __x86_64__
#define ENDBR_IMMEDIATE 0xfa1e0ff3
#define EXTEND_ENDBR_IMMEDIATE 0xfa1e0ff300000000
#else
#define ENDBR_IMMEDIATE 0xfb1e0ff3
#define EXTEND_ENDBR_IMMEDIATE 0xfffb1e0ff300
#endif

int
foo (int a)
{
  return a + ENDBR_IMMEDIATE;
}

int
foo2 (int a)
{
  return a - ENDBR_IMMEDIATE;
}

int
foo3 (int a)
{
  return a * ENDBR_IMMEDIATE;
}

int
foo4 (int a)
{
  return a | ENDBR_IMMEDIATE;
}

int
foo5 (int a)
{
  return a ^ ENDBR_IMMEDIATE;
}

int
foo6 (int a)
{
  return a & ENDBR_IMMEDIATE;
}

int
foo7 (int a)
{
  return a > ENDBR_IMMEDIATE;
}

int
foo8 (int a)
{
  return ENDBR_IMMEDIATE;
}

int
foo9 (int* p)
{
  return *(p + ENDBR_IMMEDIATE);
}

int
foo10 (int* p)
{
  return *(int*) ENDBR_IMMEDIATE;
}

long long
foo11 (long long a)
{
  return a + EXTEND_ENDBR_IMMEDIATE;
}

long long
foo12 (long long a)
{
  return a - EXTEND_ENDBR_IMMEDIATE;
}

long long
foo13 (long long a)
{
  return a * EXTEND_ENDBR_IMMEDIATE;
}

long long
foo14 (long long a)
{
  return a | EXTEND_ENDBR_IMMEDIATE;
}

long long
foo15 (long long a)
{
  return a ^ EXTEND_ENDBR_IMMEDIATE;
}

long long
foo16 (long long a)
{
  return a & EXTEND_ENDBR_IMMEDIATE;
}

long long
foo17 (long long a)
{
  return a > EXTEND_ENDBR_IMMEDIATE;
}

long long
foo18 (long long a)
{
  return EXTEND_ENDBR_IMMEDIATE;
}

long long
foo19 (long long* p)
{
  return *(p + EXTEND_ENDBR_IMMEDIATE);
}

long long
foo20 (long long* p)
{
  return *(long long*) EXTEND_ENDBR_IMMEDIATE;
}

long long
foo21 (int a)
{
  return a + ENDBR_IMMEDIATE;
}

long long
foo22 (int a)
{
  return a - ENDBR_IMMEDIATE;
}

long long
foo23 (long long a)
{
  return a * ENDBR_IMMEDIATE;
}

long long
foo24 (int a)
{
  return a | ENDBR_IMMEDIATE;
}

long long
foo25 (int a)
{
  return a ^ ENDBR_IMMEDIATE;
}

long long
foo26 (int a)
{
  return a & ENDBR_IMMEDIATE;
}

long long
foo27 (int a)
{
  return a > ENDBR_IMMEDIATE;
}

long long
foo28 (int a)
{
  return ENDBR_IMMEDIATE;
}

long long
foo29 (int* p)
{
  return *(p + ENDBR_IMMEDIATE);
}

long long
foo30 (int* p)
{
  return *(long long*) ENDBR_IMMEDIATE;
}
