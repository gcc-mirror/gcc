/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { *-*-darwin* } { "*" } { "" } } */
/* { dg-options "-O2 -mpointers-to-nested-functions" } */

int
call_ptr (int (func) (void))
{
  return func () + 1;
}

/* { dg-final { scan-assembler "ld 11,16" } } */
