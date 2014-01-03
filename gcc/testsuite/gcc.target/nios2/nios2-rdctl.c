/* { dg-do compile } */
/* { dg-final { scan-assembler "rdctl" } } */

int x ()
{
  __builtin_rdctl (0);
  return 0;
} 
