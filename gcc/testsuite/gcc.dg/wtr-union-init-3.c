/* Test for -Wtraditional warnings on union initialization.
   Note, gcc should omit these warnings in system header files.
   By Kaveh R. Ghazi <ghazi@caip.rutgers.edu> 9/11/2000.  */
/* { dg-do compile } */
/* { dg-options "-Wtraditional" } */

struct bar
{
  int i;
  long j;
};

union foo
{
  struct bar b;
  int i;
  long l;
};

union foo2
{
  int i;
  long l;
};

struct baz
{
  int a;
  double b;
  union foo c;
};

struct baz2
{
  int a;
  double b;
  union foo2 c;
};

void
testfunc ()
{
  /* Note we only warn for nonzero initializers.  Xfail on substructures. */
  static union foo f1 = {{0,0}}; /* { dg-bogus "traditional C rejects initialization of unions" "initialization of unions" { xfail *-*-* } } */
  static union foo f2 = {{1,1}}; /* { dg-warning "traditional C rejects initialization of unions" "initialization of unions" } */

  static struct baz f3 = { 1, 2, {{0,0}} }; /* { dg-bogus "traditional C rejects initialization of unions" "initialization of unions" { xfail *-*-* } } */
  static struct baz f4 = { 1, 2, {{1,1}} }; /* { dg-warning "traditional C rejects initialization of unions" "initialization of unions" } */

  static struct baz2 f5 = { 1, 2, {0} };
  static struct baz2 f6 = { 1, 2, {1} }; /* { dg-warning "traditional C rejects initialization of unions" "initialization of unions" } */

# 54 "sys-header.h" 3
/* We are in system headers now, no -Wtraditional warnings should issue.  */

  static union foo b1 = {{0,0}};
  static union foo b2 = {{1,1}};

  static struct baz b3 = { 1, 2, {{0,0}} };
  static struct baz b4 = { 1, 2, {{1,1}} };

  static struct baz2 b5 = { 1, 2, {0} };
  static struct baz2 b6 = { 1, 2, {1} };
}
