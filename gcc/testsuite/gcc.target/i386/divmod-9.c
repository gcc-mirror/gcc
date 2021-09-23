/* { dg-do compile } */
/* { dg-options "-O2" } */

int foo (int x)
{
  return 100/x;
}

int bar(int x)
{
  return -100/x;
}
/* { dg-final { scan-assembler-not "(cltd|cdq)" } } */

