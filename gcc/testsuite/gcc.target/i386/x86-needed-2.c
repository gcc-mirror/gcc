/* { dg-do compile } */
/* { dg-options "-fcf-protection=none -march=x86-64 -mno-needed" } */
/* { dg-final { scan-assembler-not ".note.gnu.property" } } */

extern void foo (void);

void
bar (void)
{
  foo ();
}
