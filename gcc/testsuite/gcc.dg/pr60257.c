/* PR c/60257 */
/* { dg-do compile } */
/* { dg-options "-Wc++-compat -Woverride-init" } */
/* { dg-prune-output ".*near initialization for.*" } */

enum E1 { A };
enum E2 { B };

struct S
{
  enum E1 e: 3;
};

struct S s[] =
{
  { B } /* { dg-warning "5:enum conversion in initialization is invalid in C\[+\]\[+\]" } */
};

union U {
  int i;
  long long int l;
};

struct R {
  int a;
};

void
foo (int i)
{
  union U u = { .i = ++i, .l = 1 }; /* { dg-warning "32:initialized field with side-effects overwritten" } */
  union U u2 = { .i = 1, .l = 3 }; /* { dg-warning "31:initialized field overwritten" } */
  int a[] = { i++, [0] = 1 }; /* { dg-warning "26:initialized field with side-effects overwritten" } */
  int a2[] = { i, [0] = 1 }; /* { dg-warning "25:initialized field overwritten" } */
  struct R r = { 1, .a = 2 }; /* { dg-warning "26:initialized field overwritten" } */
  struct R r2 = { ++i, .a = 2 }; /* { dg-warning "29:initialized field with side-effects overwritten" } */
}
