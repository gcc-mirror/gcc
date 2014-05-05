/* { dg-do compile } */
/* { dg-options "-O3 -march=z900 -pg" } */

int
foo ()
{
}
/* Make sure no stack frame is generated.  */
/* { dg-final { scan-assembler-not "ahi" { target s390-*-* } } } */
/* { dg-final { scan-assembler-not "aghi" { target s390x-*-* } } } */
