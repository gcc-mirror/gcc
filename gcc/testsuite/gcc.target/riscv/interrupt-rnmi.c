/* Verify the return instruction is mnret.  */
/* { dg-do compile } */
/* { dg-options "-march=rv32gc_smrnmi" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_smrnmi" { target { rv64 } } } */

void __attribute__ ((interrupt ("rnmi")))
foo (void)
{
}

/* { dg-final { scan-assembler {\mmnret} } } */
