/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64" } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
**my_crc:
**	crc.w.d.w	\$r4,\$r4,\$r5
**	jr	\$r1
*/
int my_crc(long long dword, int crc)
{
	return __builtin_loongarch_crc_w_d_w(dword, crc);
}
