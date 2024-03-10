/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-fcf-protection=return,branch" } */
/* { dg-final { scan-assembler-times ".note.gnu.property" 1 } } */
/* { dg-final { scan-assembler-times ".long	0x3" 1 } } */
