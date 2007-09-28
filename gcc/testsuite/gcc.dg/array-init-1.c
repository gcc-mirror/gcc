/* Test that both arrays are initialized by store_by_pieces.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -mtune=i686" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */

struct A { char c[10]; };
extern void baz (struct A *);

void
foo (void)
{
  struct A a = { "abcdefghi" };
  baz (&a);
}

void
bar (void)
{
  struct A a;
  __builtin_strcpy (&a.c[0], "abcdefghi");
  baz (&a);
}

/* { dg-final { scan-assembler-not "abcdefghi" { target i?86-*-* x86_64-*-* ia64-*-* } } } */
/* { dg-final { scan-assembler-times "7523094288207667809\|6867666564636261\|1684234849\|64636261" 2 { target i?86-*-* x86_64-*-* ia64-*-* } } } */
