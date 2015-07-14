/* { dg-do compile } */
/* { dg-final { scan-assembler "rdprs" } } */

int x ()
{
  __builtin_rdprs (3,934);
  return 0;
} 
