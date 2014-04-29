/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "addc" 1 } } */
/* { dg-final { scan-assembler-times "adde" 1 } } */
/* { dg-final { scan-assembler-times "subfc" 1 } } */
/* { dg-final { scan-assembler-times "subfe" 1 } } */
/* { dg-final { scan-assembler-not "subf " } } */

__int128
add_128 (__int128 *ptr, __int128 val)
{
	return (*ptr + val);
}

__int128
sub_128 (__int128 *ptr, __int128 val)
{
	return (*ptr - val);
}

