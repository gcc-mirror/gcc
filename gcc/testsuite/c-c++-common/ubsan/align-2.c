/* Limit this to known non-strict alignment targets.  */
/* { dg-do run { target { i?86-*-linux* x86_64-*-linux* } } } */
/* { dg-options "-fsanitize=alignment -Wno-address-of-packed-member" } */

struct S { int a; char b; long long c; short d[10]; };
struct T { char a; long long b; };
struct U { char a; int b; int c; long long d; struct S e; struct T f; } __attribute__((packed));
struct V { long long a; struct S b; struct T c; struct U u; } v;

__attribute__((noinline, noclone)) void
f1 (int *p, int *q, char *r, long long *s)
{
  *p =
      *q
      + *r
      + *s;
}


__attribute__((noinline, noclone)) int
f2 (struct S *p)
{
  return p->a;
}

__attribute__((noinline, noclone)) long long
f3 (struct S *p, int i)
{
  return p->c
	 + p->d[1]
	 + p->d[i];
}

__attribute__((noinline, noclone)) long long
f4 (long long *p)
{
  return *p;
}

int
main ()
{
  f1 (&v.u.b, &v.u.c, &v.u.a, &v.u.d);
  if (f2 (&v.u.e) + f3 (&v.u.e, 4) + f4 (&v.u.f.b) != 0)
    __builtin_abort ();
  return 0;
}

/* { dg-output "\.c:(14|15):\[0-9]*: \[^\n\r]*load of misaligned address 0x\[0-9a-fA-F]* for type 'int', which requires 4 byte alignment.*" } */
/* { dg-output "\.c:16:\[0-9]*: \[^\n\r]*load of misaligned address 0x\[0-9a-fA-F]* for type 'long long int', which requires \[48] byte alignment.*" } */
/* { dg-output "\.c:(13|16):\[0-9]*: \[^\n\r]*store to misaligned address 0x\[0-9a-fA-F]* for type 'int', which requires 4 byte alignment.*" } */
/* { dg-output "\.c:23:\[0-9]*: \[^\n\r]*member access within misaligned address 0x\[0-9a-fA-F]* for type 'struct S', which requires \[48] byte alignment.*" } */
/* { dg-output "\.c:(29|30):\[0-9]*: \[^\n\r]*member access within misaligned address 0x\[0-9a-fA-F]* for type 'struct S', which requires \[48] byte alignment.*" } */
/* { dg-output "\.c:30:\[0-9]*: \[^\n\r]*member access within misaligned address 0x\[0-9a-fA-F]* for type 'struct S', which requires \[48] byte alignment.*" } */
/* { dg-output "\.c:31:\[0-9]*: \[^\n\r]*member access within misaligned address 0x\[0-9a-fA-F]* for type 'struct S', which requires \[48] byte alignment.*" } */
/* { dg-output "\.c:37:\[0-9]*: \[^\n\r]*load of misaligned address 0x\[0-9a-fA-F]* for type 'long long int', which requires \[48] byte alignment" } */
