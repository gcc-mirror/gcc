/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */

unsigned int get(void)
{
       return __builtin_xtensa_get_fsr();
}

void set(unsigned int fsr)
{
	__builtin_xtensa_set_fsr(fsr);
}

/* { dg-final { scan-assembler-times "rur.fsr" 1 } } */
/* { dg-final { scan-assembler-times "wur.fsr" 1 } } */
