/* Derive a constant (0x1233ffff) from an intermediate value
   (0x1234000) used to build another constant.  */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler-not "0x12330000|305332224" } } */
/* { dg-final { scan-assembler "\td?addiu\t\\\$4,\\\$\[0-9\]*,-1" } } */

extern void g (int, int);

NOMIPS16 void f ()
{
  g (0x1233ffff, 0x12340001);
}
