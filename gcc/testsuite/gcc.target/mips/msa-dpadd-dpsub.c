/* { dg-do compile } */
/* { dg-options "-mfp64 -mhard-float -mmsa" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef short v8i16 __attribute__ ((vector_size (16)));
typedef int v4i32 __attribute__ ((vector_size (16)));

void foo (int *x, v8i16 *y, v8i16 *z)
{
	v4i32 acc[4];

    acc[0] = __builtin_msa_ld_w(x, 0);
    acc[1] = __builtin_msa_ld_w(x, 16);
    acc[2] = __builtin_msa_ld_w(x, 32);
    acc[3] = __builtin_msa_ld_w(x, 48);
		
    acc[0] = __builtin_msa_dpadd_s_w(acc[0], y[0], z[0]);
    acc[1] = __builtin_msa_dpadd_s_w(acc[1], y[1], z[0]);
    acc[2] = __builtin_msa_dpsub_s_w(acc[2], y[0], z[1]);
    acc[3] = __builtin_msa_dpsub_s_w(acc[3], y[1], z[1]);

    __builtin_msa_st_w(acc[0], x, 0);
    __builtin_msa_st_w(acc[1], x, 16);
    __builtin_msa_st_w(acc[2], x, 32);
    __builtin_msa_st_w(acc[3], x, 48);
}

/* { dg-final { scan-assembler-not "move.v" } } */
