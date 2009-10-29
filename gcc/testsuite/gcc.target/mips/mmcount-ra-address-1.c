/* { dg-do compile } */
/* { dg-options "-O2 -pg -mmcount-ra-address -mabi=64" } */
/* { dg-final { scan-assembler "\tmove\t\\\$12,\\\$0" } } */
int bazl(int i)
{
  return i + 2;
}
