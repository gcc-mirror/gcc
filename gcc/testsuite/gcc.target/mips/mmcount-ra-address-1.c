/* { dg-do compile } */
/* { dg-options "-pg -mmcount-ra-address -mabi=64" } */
/* { dg-final { scan-assembler "\tmove\t\\\$12,\\\$0" } } */
NOMIPS16 int bazl(int i)
{
  return i + 2;
}
