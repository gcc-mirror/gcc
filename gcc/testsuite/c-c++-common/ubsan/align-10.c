/* Limit this to known non-strict alignment targets.  */
/* { dg-do run { target { i?86-*-linux* x86_64-*-linux* } } } */
/* { dg-options "-O -fsanitize=alignment -fsanitize-recover=alignment -Wno-address-of-packed-member" } */

struct R { int a; } r;
struct S { struct R a; char b; long long c; short d[10]; };
struct T { char a; long long b; };
struct U { char a; int b; int c; long long d; struct S e; struct T f; } __attribute__((packed));
struct V { long long a; struct S b; struct T c; struct U u; } v;

__attribute__((noinline, noclone)) int
bar (int x, struct R y, struct R z)
{
  return x + y.a;
}

__attribute__((noinline, noclone)) int
foo (struct S *p, struct S *q)
{
  int i = bar (0, r, r);
  i += bar (1, p->a, r);
  i += bar (2, r, q->a);
  return i;
}

int
main ()
{
  char *p = (char *) &v.u.e;
  struct S *q, *r;
  asm volatile ("" : "=r" (q) : "0" (p));
  asm volatile ("" : "=r" (r) : "0" (p));
  if (foo (q, r) != 3)
    __builtin_abort ();
  return 0;
}

/* { dg-output "\.c:21:\[0-9]*: \[^\n\r]*member access within misaligned address 0x\[0-9a-fA-F]* for type 'struct S', which requires \[48] byte alignment.*" } */
/* { dg-output "\.c:22:\[0-9]*: \[^\n\r]*member access within misaligned address 0x\[0-9a-fA-F]* for type 'struct S', which requires \[48] byte alignment" } */
