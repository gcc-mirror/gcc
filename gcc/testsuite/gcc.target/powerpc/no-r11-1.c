/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { *-*-darwin* } { "*" } { "" } } */
/* { dg-skip-if "" { powerpc_elfv2 } { "*" } { "" } } */
/* { dg-options "-O2 -mno-pointers-to-nested-functions" } */

int
call_ptr (int (func) (void))
{
  return func () + 1;
}

/* { dg-final { scan-assembler-not "ld 11,16(3)" } } */
