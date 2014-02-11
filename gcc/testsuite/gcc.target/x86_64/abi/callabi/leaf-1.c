/* { dg-do compile } */
/* { dg-options "-O2 -mabi=sysv" } */

__attribute__ ((ms_abi))
int foo (void)
{
  return 0;
}

/* { dg-final { scan-assembler-not "%rsp" } } */

