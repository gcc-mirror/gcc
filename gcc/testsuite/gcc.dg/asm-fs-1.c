/* Origin: <hp@bitrange.com>
   Make sure we do not get spurious '*' characters in section names or
   elsewhere, with asm-specified names.  */
/* { dg-do compile } */
/* { dg-options "-w -ffunction-sections -fdata-sections" } */

void foo (void) asm ("_bar");
void foo (void) {}

extern int foobar asm ("_baz");
int foobar = 3;

/* { dg-final { scan-assembler-not "\\*_bar" } } */
/* { dg-final { scan-assembler-not "\\*_baz" } } */
