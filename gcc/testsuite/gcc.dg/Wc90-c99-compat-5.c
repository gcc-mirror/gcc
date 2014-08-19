/* { dg-do compile } */
/* { dg-options "-std=gnu90 -pedantic-errors -Wno-c90-c99-compat" } */

_Bool b;
_Complex double c = __builtin_complex (0.0, 0.0);
long long l;
struct A { int i; char a[]; };
struct { long int b: 2; } s;
const const int i;
volatile volatile int v;

struct S { int a[2]; };
extern struct S foo (void);

enum { E, };

void fn1 (char [*]);

void
fn2 (char x[static 4])
{
  int i = (int) { 1 };
  struct A a = { .i = 3 };
}

void
fn3 (int n)
{
  n = 3;
  int i;
}

void
fn4 (int n)
{
  n = 3;
  __extension__ int i;
}

void
fn5 (void)
{
  (foo ()).a[0];
}

#define F(a) a

void
fn6 (void)
{
  F();
}

void fn7 (int n, int a[n]);
