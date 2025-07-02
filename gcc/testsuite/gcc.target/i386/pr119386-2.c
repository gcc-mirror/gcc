/* PR target/119386 */
/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpic -fno-plt -pg" } */
/* { dg-additional-options "-mfentry" { target { ! ia32 }  } } */
/* { dg-final { scan-assembler "call\[ \t\]+\\*__fentry__@GOTPCREL" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "call\[ \t\]+\\*mcount@GOT\\(" { target ia32 } } } */


int
main ()
{
  return 0;
}
