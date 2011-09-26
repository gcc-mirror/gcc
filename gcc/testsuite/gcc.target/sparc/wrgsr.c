/* { dg-do compile } */
/* { dg-options "-O -mcpu=ultrasparc -mvis" } */

void set_gsr (void)
{
	__builtin_vis_write_gsr (2 << 3);
}

void set_gsr2 (long x)
{
	__builtin_vis_write_gsr (x);
}

/* { dg-final { scan-assembler "wr\t%g0, 16, %gsr" } } */
/* { dg-final { scan-assembler "wr\t%g0, %" } } */
