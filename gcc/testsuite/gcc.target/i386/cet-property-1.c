/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-fcf-protection" } */
/* { dg-final { scan-assembler ".note.gnu.property" } } */

extern void foo (void);

void
bar (void)
{
  foo ();
}
