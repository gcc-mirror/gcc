// Limit this to known non-strict alignment targets.
// { dg-do run { target { i?86-*-linux* x86_64-*-linux* } } }
// { dg-options "-fsanitize=alignment -Wall -Wno-unused-variable -std=c++11" }

typedef const long int L;
struct S { long int l; char buf[1 + sizeof (int) + sizeof (L)]; } s;
struct T { char a; int b; long int c; } __attribute__((packed));
struct U { long int a; struct T b; } u;

int
main (void)
{
  int *p = (int *) &s.buf[1];
  L *l = (L *) &s.buf[1 + sizeof(int)];

  int &r = *p;
  auto &r2 = *p;
  L &lr = *l;

  // Try an rvalue reference.
  auto &&r3 = *p;

  // Don't evaluate the reference initializer twice.
  int i = 1;
  int *q = &i;
  int &qr = ++*q;
  if (i != 2)
    __builtin_abort ();

  int *s = &u.b.b;
  L *t = &u.b.c;
  int &r4 = *s;
  auto &r5 = *s;
  L &lr2 = *t;
  auto &&r6 = *s;
}

// { dg-output "\.C:16:\[0-9]*:\[\^\n\r]*reference binding to misaligned address 0x\[0-9a-fA-F]* for type 'int', which requires 4 byte alignment.*" }
// { dg-output "\.C:17:\[0-9]*:\[\^\n\r]*reference binding to misaligned address 0x\[0-9a-fA-F]* for type 'int', which requires 4 byte alignment.*" }
// { dg-output "\.C:18:\[0-9]*:\[\^\n\r]*reference binding to misaligned address 0x\[0-9a-fA-F]* for type 'const L', which requires \[48] byte alignment.*" }
// { dg-output "\.C:21:\[0-9]*:\[\^\n\r]*reference binding to misaligned address 0x\[0-9a-fA-F]* for type 'int', which requires 4 byte alignment.*" }
// { dg-output "\.C:32:\[0-9]*:\[\^\n\r]*reference binding to misaligned address 0x\[0-9a-fA-F]* for type 'int', which requires 4 byte alignment.*" }
// { dg-output "\.C:33:\[0-9]*:\[\^\n\r]*reference binding to misaligned address 0x\[0-9a-fA-F]* for type 'int', which requires 4 byte alignment.*" }
// { dg-output "\.C:34:\[0-9]*:\[\^\n\r]*reference binding to misaligned address 0x\[0-9a-fA-F]* for type 'const L', which requires \[48] byte alignment.*" }
// { dg-output "\.C:35:\[0-9]*:\[\^\n\r]*reference binding to misaligned address 0x\[0-9a-fA-F]* for type 'int', which requires 4 byte alignment" }
