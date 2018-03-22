/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-fcf-protection -mcet" } */
/* { dg-final { scan-assembler ".note.gnu.property" } } */

extern void foo (void);

void
bar (void)
{
  foo ();
}
