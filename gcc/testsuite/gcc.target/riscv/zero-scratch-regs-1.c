/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64 -O2 -fzero-call-used-regs=used -fno-stack-protector -fno-PIC" } */

void
foo (void)
{
}

/* { dg-final { scan-assembler-not "li\t" } } */
