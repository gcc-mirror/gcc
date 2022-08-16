/* PR rtl-optimization/96539 */
/* { dg-do compile } *
/* { dg-options "-Os" } */
/* The need to restore the PIC register prevents PLT tail-calls on ia32,
   so S has to be copied to call baz.  */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */
/* { dg-final { scan-assembler-not "rep\[^\n\r]\*movs" } } */

struct A { int a, b, c, d, e, f; void *g, *h, *i, *j, *k, *l, *m; };

int bar (int a);
int baz (int a, int b, int c, void *p, struct A s);

int
foo (int a, int b, int c, void *p, struct A s)
{
  bar (a);
  return baz (a, b, c, p, s);
}
