/* Derive a constant (0x30001) from another constant.  */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* See PR61926 for the XFAILs.  */
/* { dg-final { scan-assembler-not "0x300000|196608" { xfail *-*-* } } } */
/* { dg-final { scan-assembler "\td?addiu\t\\\$5,\\\$\[0-9\]*,32763" { xfail *-*-* }  } } */

extern void g (int, int);

NOMIPS16 void f ()
{
  g (0x28006, 0x30001);
}
