/* { dg-do compile } */
/* { dg-options "-mcet" } */
/* { dg-final { scan-assembler-not ".note.gnu.property" } } */

extern void foo (void);

void
bar (void)
{
  foo ();
}
