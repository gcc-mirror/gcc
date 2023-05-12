/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-fcf-protection=return,none" } */
/* { dg-final { scan-assembler-times ".note.gnu.property" 1 } } */
/* { dg-final { scan-assembler-times ".long	0x2" 1 } } */
