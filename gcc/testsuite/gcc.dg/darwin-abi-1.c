/* { dg-do compile { target powerpc*-*-darwin* } } */
/* { dg-options "-O" } */
/* { dg-final { scan-assembler "li r3,12345\n\tbl " } } */

/* Check that zero-size structures don't affect parameter passing.  */

struct empty { };
extern void foo (struct empty e, int a);
void bar (void) {
  struct empty e;
  foo (e, 12345);
}
