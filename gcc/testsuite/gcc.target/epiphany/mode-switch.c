/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "#-917506" } } */
/* PR 60651 */

int a;
int c;

void __attribute__((interrupt))
misc_handler (void) {
   a*= c;
}
