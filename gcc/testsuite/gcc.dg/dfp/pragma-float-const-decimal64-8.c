/* { dg-do compile } */
/* { dg-options "-Wall" } */

/* N1312 7.1.1: The FLOAT_CONST_DECIMAL64 pragma.
   C99 6.4.4.2a (New).

   Pragma STDC FLOAT_CONST_DECIMAL64 "shall occur either outside external
   declarations or preceding all explicit declarations and statements
   inside a compound statement." */

#pragma STDC FLOAT_CONST_DECIMAL64 OFF

#define MAX 200

#pragma STDC FLOAT_CONST_DECIMAL64 ON

double a;

#pragma STDC FLOAT_CONST_DECIMAL64 OFF

struct S1 {
#pragma STDC FLOAT_CONST_DECIMAL64 ON	/* { dg-warning "invalid location" } */
  int i;
  int j;
};

struct S2 {
  int i;
#pragma STDC FLOAT_CONST_DECIMAL64 ON	/* { dg-warning "invalid location" } */
  int j;
};

struct S3 {
  int i;
  int j;
#pragma STDC FLOAT_CONST_DECIMAL64 ON	/* { dg-warning "invalid location" } */
};

enum E1 {
#pragma STDC FLOAT_CONST_DECIMAL64 ON	/* { dg-error "#pragma" } */
  one,
  two
};

enum E2 {
  red,
#pragma STDC FLOAT_CONST_DECIMAL64 ON	/* { dg-error "#pragma" } */
  blue
};

enum E3 {
  cat,
  dog
#pragma STDC FLOAT_CONST_DECIMAL64 ON	/* { dg-error "#pragma" } */
};

double
#pragma STDC FLOAT_CONST_DECIMAL64 OFF	/* { dg-error "#pragma" } */
b;

double
f1 (void)
{
#pragma STDC FLOAT_CONST_DECIMAL64 ON
  return a;
}

double
f2 (void)
{
  double b;
#pragma STDC FLOAT_CONST_DECIMAL64 ON	/* { dg-warning "invalid location" } */
  b = 0.5;
  return a + b;
}

#pragma STDC FLOAT_CONST_DECIMAL64 OFF

double
f3 (void)
{
  typedef double b32;
#pragma STDC FLOAT_CONST_DECIMAL64 ON	/* { dg-warning "invalid location" } */
  b32 b = 0.5;
  return b;
}

double
f4 (int i)
{
top:
#pragma STDC FLOAT_CONST_DECIMAL64 OFF	/* { dg-warning "invalid location" } */
  if (i == 0)
    return a;  
  a *= 2.;
  i = 0;
  goto top;
}

double
f5 (int i)
{
  a = a * i;
#pragma STDC FLOAT_CONST_DECIMAL64 OFF	/* { dg-warning "invalid location" } */
  return a * 2.;
}

double
#pragma STDC FLOAT_CONST_DECIMAL64 ON	/* { dg-error "#pragma" } */
f6 (void)
{
  return a;
}

double
f7
#pragma STDC FLOAT_CONST_DECIMAL64 ON	/* { dg-error "#pragma" } */
(void)					/* { dg-error "before" } */
{
  return a;
}

double
f8 (void)
{
  {
#pragma STDC FLOAT_CONST_DECIMAL64 OFF
  }
#pragma STDC FLOAT_CONST_DECIMAL64 ON   /* { dg-warning "invalid location" } */
  return a;
}

extern void foo9 (void *);

double
f9 (void)
{
  __label__ here;
#pragma STDC FLOAT_CONST_DECIMAL64 ON	/* { dg-warning "invalid location" } */
  foo9 (&&here);
here:
  return a;
}

double
f10 (void)
{
  void foo10 (void)
  {
    a = 1.0;
  }
#pragma STDC FLOAT_CONST_DECIMAL64 ON	/* { dg-warning "invalid location" } */
  return a;
}

double
f11 (void)
{
  __extension__
   struct A {
    struct { char a; };
    char b;
  };
#pragma STDC FLOAT_CONST_DECIMAL64 ON	/* { dg-warning "invalid location" } */
  return a;
}

double
f12 (void)
{
  __extension__ ({ a = 0.5; });
#pragma STDC FLOAT_CONST_DECIMAL64 ON	/* { dg-warning "invalid location" } */
  return a;
}
