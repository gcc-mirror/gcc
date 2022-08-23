/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fplt -fpic -mno-direct-extern-access" } */

extern void foo (void);

int
bar (void)
{
  foo ();
  return 0;
}

/* { dg-final { scan-assembler "call\[ \t\]*foo@PLT" } } */
/* { dg-final { scan-assembler-not "foo@GOTPCREL" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "foo@GOT" { target ia32 } } } */
/* { dg-final { scan-assembler "\.section\[ \t]+.note.gnu.property," } } */
/* { dg-final { scan-assembler "\.long\[ \t]+0xb0008000" } } */

