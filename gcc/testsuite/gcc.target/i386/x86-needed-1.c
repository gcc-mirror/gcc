/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-fcf-protection -march=x86-64 -mneeded" } */
/* { dg-final { scan-assembler-times ".note.gnu.property" 1 } } */
/* { dg-final { scan-assembler-times ".long	0xc0000002" 1 } } */
/* { dg-final { scan-assembler-times ".long	0xc0008002" 1 } } */

extern void foo (void);

void
bar (void)
{
  foo ();
}
