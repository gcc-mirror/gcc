/* { dg-do compile { target ia32 } } */
/* { dg-options "-fcf-protection=none -march=i686 -msoft-float -mneeded" } */
/* { dg-final { scan-assembler-not ".note.gnu.property" } } */

extern void foo (void);

void
bar (void)
{
  foo ();
}
