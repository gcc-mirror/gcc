/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-mcpu=ck801" "-march=ck801" } { "*" } } */
/* { dg-csky-options "-O1" } */

int foo (signed char *pb)
{
  return *pb;
}

/* { dg-final { scan-assembler "ld.bs" } } */

