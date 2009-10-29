/* { dg-do compile } */
/* { dg-options "-O2 -pg -mmcount-ra-address -mabi=64" } */
/* { dg-final { scan-assembler "\tdla\t\\\$12,8\\(\\\$sp\\)" } } */
int foo (int);
int bar (int i)
{
  return foo (i) + 2;
}
