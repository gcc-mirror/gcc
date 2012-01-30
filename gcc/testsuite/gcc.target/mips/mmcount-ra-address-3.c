/* { dg-do compile } */
/* { dg-options "-O2 -pg -mmcount-ra-address -mabi=64" } */
/* { dg-final { scan-assembler "\tdla\t\\\$12,200008\\(\\\$sp\\)" } } */
int foo (int *);
NOMIPS16 int bar(int i)
{
  int big[50000];
  return foo (big) + 2;
}
