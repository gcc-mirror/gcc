/* Derive a constant (0x30001) from another constant.  */
/* { dg-options "-O" } */
/* { dg-final { scan-assembler-not "0x300000|196608" } } */
/* { dg-final { scan-assembler "addiu\t\\\$5,\\\$\[0-9\]*,32763" } } */

NOMIPS16 void f ()
{
  g (0x28006, 0x30001);
}
