/* { dg-do compile { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
/* { dg-options "-march=rv32gc -mabi=ilp32d -O0" } */
/* { dg-final { check-function-bodies "**" "" } } */

typedef long long __int64;

__int64 foo ()
{
/*
** foo:
**   ...
**   li\t[atx][0-9]+,1
**   li\t[atx][0-9]+,1
**   ...
*/
  __int64 ret;
  asm ("li\t%0,1\n\tli\t%H0,1\n\t":"=r"(ret));

  return ret;
}

