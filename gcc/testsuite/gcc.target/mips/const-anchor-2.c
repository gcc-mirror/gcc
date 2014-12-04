/* Derive a constant (0x30001) from another constant.  */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler-not "0x300000|196608" } } */
/* { dg-final { scan-assembler "\td?addiu\t\\\$4,\\\$\[0-9\]*,32763" } } */

extern void g (int, int);

NOMIPS16 void f ()
{
  g (0x30001, 0x28006);
}
