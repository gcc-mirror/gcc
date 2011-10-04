/* { dg-do compile } */
/* { dg-options "-O -mcpu=ultrasparc -mvis" } */

long get_gsr (void)
{
	return __builtin_vis_read_gsr ();
}

/* { dg-final { scan-assembler "rd\t%gsr" } } */
