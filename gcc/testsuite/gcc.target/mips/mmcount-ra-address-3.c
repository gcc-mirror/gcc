/* { dg-do compile } */
/* { dg-options "-pg -mmcount-ra-address -mabi=64 -mno-abicalls" } */
/* { dg-skip-if "requiring a specific frame layout makes this a code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler "\tdla\t\\\$12,200008\\(\\\$sp\\)" } } */
int foo (int *);
NOMIPS16 int bar(int i)
{
  int big[50000];
  return foo (big) + 2;
}
