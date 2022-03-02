/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpic -mdirect-extern-access" } */

/* Common symbol with -fpic.  */
__attribute__((visibility("protected"), nodirect_extern_access))
int xxx;

int
foo ()
{
  return xxx;
}

/* { dg-final { scan-assembler "xxx\\(%rip\\)" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "xxx@GOTPCREL" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "xxx@GOTOFF" { target ia32 } } } */
/* { dg-final { scan-assembler-not "xxx@GOT\\(" { target ia32 } } } */
/* { dg-final { scan-assembler "\.section\[ \t]+.note.gnu.property," } } */
/* { dg-final { scan-assembler "\.long\[ \t]+0xb0008000" } } */

