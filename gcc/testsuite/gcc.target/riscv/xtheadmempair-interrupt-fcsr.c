/* Verify that fcsr instructions emitted.  */
/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-g" "-Oz" "-Os" "-flto" } } */
/* { dg-options "-march=rv64gc_xtheadmempair -mtune=thead-c906 -funwind-tables" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_xtheadmempair -mtune=thead-c906 -funwind-tables" { target { rv32 } } } */


extern int foo (void);

void __attribute__ ((interrupt))
sub (void)
{
  foo ();
}

/* { dg-final { scan-assembler-times "frcsr\t" 1 } } */
/* { dg-final { scan-assembler-times "fscsr\t" 1 } } */
