/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-fno-pic" } */

__attribute__ ((noplt))
void foo();

int main()
{
  foo();
  return 0;
}

/* { dg-final { scan-assembler "call\[ \t\]*.foo@GOTPCREL" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "call\[ \t\]*.foo@GOT" { target { ia32 && got32x_reloc } } } } */
