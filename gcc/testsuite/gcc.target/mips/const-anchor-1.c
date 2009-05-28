/* Derive a constant (0x1233ffff) from an intermediate value
   (0x1234000) used to build another constant.  */
/* { dg-options "-O" } */
/* { dg-final { scan-assembler-not "0x12330000|305332224" } } */
/* { dg-final { scan-assembler "addiu\t\\\$5,\\\$\[0-9\]*,-1" } } */

NOMIPS16 void f ()
{
  g (0x12340001, 0x1233ffff);
}
