/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fno-pic" } */


__attribute__ ((noplt))
int foo();

int bar()
{
  return foo();
}

/* { dg-final { scan-assembler "jmp\[ \t\]*.foo@GOTPCREL" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "jmp\[ \t\]*.foo@GOT" { target { ia32 && got32x_reloc } } } } */
