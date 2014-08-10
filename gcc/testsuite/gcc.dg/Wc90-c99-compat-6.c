/* { dg-do compile } */
/* { dg-options "-std=gnu90 -Wc90-c99-compat" } */

__extension__ _Bool b;
__extension__ _Complex double c = __builtin_complex (0.0, 0.0);
__extension__ long long l;
__extension__ struct A { int i; char a[]; };
__extension__ struct { long int b: 2; } s;
__extension__ const const int i;
__extension__ volatile volatile int v;
__extension__ struct S { int a[2]; };
extern struct S foo (void);
__extension__ enum { E, };
__extension__ void fn1 (char [*]);

__extension__ void
fn2 (char x[static 4])
{
  int i = (int) { 1 };
  struct A a = { .i = 3 };
}

__extension__ void
fn5 (void)
{
  (foo ()).a[0];
}

__extension__ void fn7 (int n, int a[n]);
