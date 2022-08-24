/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fplt -fno-pic -mno-direct-extern-access" } */

extern void foo (void);

int
bar (void)
{
  foo ();
  return 0;
}

/* { dg-final { scan-assembler "call\[ \t\]*foo" } } */
/* { dg-final { scan-assembler-not "foo@GOTPCREL" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "foo@GOT" { target ia32 } } } */
/* { dg-final { scan-assembler "\.section\[ \t]+.note.gnu.property," } } */
/* { dg-final { scan-assembler "\.long\[ \t]+0xb0008000" } } */

