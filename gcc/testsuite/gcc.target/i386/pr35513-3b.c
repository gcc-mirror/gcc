/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpie -mdirect-extern-access" } */

extern int bar __attribute__ ((nodirect_extern_access));

int
foo (void)
{
  return bar;
}

/* { dg-final { scan-assembler "mov\(l|q\)\[ \t\]*bar@GOTPCREL" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "movl\[ \t\]*bar@GOT" { target { ia32 && got32x_reloc } } } } */
/* { dg-final { scan-assembler-not "mov\(l|q\)\[ \t\]*\\\$bar," { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "mov\(l|q\)\[ \t\]*\\\$bar," { target { ia32 && got32x_reloc } } } } */
/* { dg-final { scan-assembler "\.section\[ \t]+.note.gnu.property," } } */
/* { dg-final { scan-assembler "\.long\[ \t]+0xb0008000" } } */

