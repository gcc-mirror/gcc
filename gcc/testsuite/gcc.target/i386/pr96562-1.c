/* { dg-do compile } */
/* { dg-options "-msse4.1 -O2" } */
/* { dg-final { scan-assembler-not "pinsr" } } */

typedef struct
{
  long long a;
  int b;
} st1;

typedef struct
{
  long long a;
  int b;
  short c;
} st2;

typedef struct
{
  long long a;
  int b;
  short c;
  char d;
} st3;

typedef struct
{
  int b;
  long long a;
} st4;

typedef struct
{
  short c;
  int b;
  long long a;
} st5;

typedef struct
{
  char d;
  short c;
  int b;
  long long a;
} st6;

st1
foo1 (long long a, int b)
{
  return (st1){a, b};
}

st2
foo2 (long long a, int b, short c)
{
  return (st2){a, b, c};
}

st3
foo3 (long long a, int b, short c, char d)
{
  return (st3){a, b, c, d};
}

st4
foo4 (long long a, int b)
{
  return (st4){b, a};
}

st5
foo5 (long long a, int b, short c)
{
  return (st5){c, b, a};
}

st6
foo6 (long long a, int b, short c, char d)
{
  return (st6){d, c, b, a};
}
