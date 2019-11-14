/* { dg-additional-options "-msve-vector-bits=scalable" } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"
#include <stdbool.h>

/*
** cnt_b8_32:
**	cntp	x0, p0, p1\.b
**	ret
*/
TEST_PTEST (cnt_b8_32, uint32_t,
	    x0 = svcntp_b8 (p0, p1));

/*
** cnt_b8_64:
**	cntp	x0, p0, p1\.b
**	ret
*/
TEST_PTEST (cnt_b8_64, uint64_t,
	    x0 = svcntp_b8 (p0, p1));

/*
** inc_b8_32_general_x0:
**	cntp	x([0-9]+), p0, p1\.b
**	add	w0, (w0, w\1|w\1, w0)
**	ret
*/
TEST_PTEST (inc_b8_32_general_x0, uint32_t,
	    x0 += svcntp_b8 (p0, p1));

/*
** inc_b8_32_general_x1:
**	cntp	x([0-9]+), p0, p1\.b
**	add	w0, (w1, w\1|w\1, w1)
**	ret
*/
TEST_PTEST (inc_b8_32_general_x1, uint32_t,
	    x0 = x1 + svcntp_b8 (p0, p1));

/*
** inc_b8_32_ptrue_x0:
**	incp	x0, p1\.b
**	ret
*/
TEST_PTEST (inc_b8_32_ptrue_x0, uint32_t,
	    x0 += svcntp_b8 (svptrue_b8 (), p1));

/*
** inc_b8_32_ptrue_x1:
**	mov	w0, w1
**	incp	x0, p1\.b
**	ret
*/
TEST_PTEST (inc_b8_32_ptrue_x1, uint32_t,
	    x0 = x1 + svcntp_b8 (svptrue_b8 (), p1));

/*
** inc_b8_64_general_x0:
**	cntp	(x[0-9]+), p0, p1\.b
**	add	x0, (x0, \1|\1, x0)
**	ret
*/
TEST_PTEST (inc_b8_64_general_x0, uint64_t,
	    x0 += svcntp_b8 (p0, p1));

/*
** inc_b8_64_general_x1:
**	cntp	(x[0-9]+), p0, p1\.b
**	add	x0, (x1, \1|\1, x1)
**	ret
*/
TEST_PTEST (inc_b8_64_general_x1, uint64_t,
	    x0 = x1 + svcntp_b8 (p0, p1));

/*
** inc_b8_64_ptrue_x0:
**	incp	x0, p1\.b
**	ret
*/
TEST_PTEST (inc_b8_64_ptrue_x0, uint64_t,
	    x0 += svcntp_b8 (svptrue_b8 (), p1));

/*
** inc_b8_64_ptrue_x1:
**	mov	x0, x1
**	incp	x0, p1\.b
**	ret
*/
TEST_PTEST (inc_b8_64_ptrue_x1, uint64_t,
	    x0 = x1 + svcntp_b8 (svptrue_b8 (), p1));

/*
** dec_b8_32_general_x0:
**	cntp	x([0-9]+), p0, p1\.b
**	sub	w0, w0, w\1
**	ret
*/
TEST_PTEST (dec_b8_32_general_x0, uint32_t,
	    x0 -= svcntp_b8 (p0, p1));

/*
** dec_b8_32_general_x1:
**	cntp	x([0-9]+), p0, p1\.b
**	sub	w0, w1, w\1
**	ret
*/
TEST_PTEST (dec_b8_32_general_x1, uint32_t,
	    x0 = x1 - svcntp_b8 (p0, p1));

/*
** dec_b8_32_ptrue_x0:
**	decp	x0, p1\.b
**	ret
*/
TEST_PTEST (dec_b8_32_ptrue_x0, uint32_t,
	    x0 -= svcntp_b8 (svptrue_b8 (), p1));

/*
** dec_b8_32_ptrue_x1:
**	mov	w0, w1
**	decp	x0, p1\.b
**	ret
*/
TEST_PTEST (dec_b8_32_ptrue_x1, uint32_t,
	    x0 = x1 - svcntp_b8 (svptrue_b8 (), p1));

/*
** dec_b8_64_general_x0:
**	cntp	(x[0-9]+), p0, p1\.b
**	sub	x0, x0, \1
**	ret
*/
TEST_PTEST (dec_b8_64_general_x0, uint64_t,
	    x0 -= svcntp_b8 (p0, p1));

/*
** dec_b8_64_general_x1:
**	cntp	(x[0-9]+), p0, p1\.b
**	sub	x0, x1, \1
**	ret
*/
TEST_PTEST (dec_b8_64_general_x1, uint64_t,
	    x0 = x1 - svcntp_b8 (p0, p1));

/*
** dec_b8_64_ptrue_x0:
**	decp	x0, p1\.b
**	ret
*/
TEST_PTEST (dec_b8_64_ptrue_x0, uint64_t,
	    x0 -= svcntp_b8 (svptrue_b8 (), p1));

/*
** dec_b8_64_ptrue_x1:
**	mov	x0, x1
**	decp	x0, p1\.b
**	ret
*/
TEST_PTEST (dec_b8_64_ptrue_x1, uint64_t,
	    x0 = x1 - svcntp_b8 (svptrue_b8 (), p1));

/*
** inc_b8_s8_general_z0:
**	cntp	x([0-9]+), p0, p1\.b
**	mov	(z[0-9]+\.b), w\1
**	add	z0\.b, (z0\.b, \2|\2, z0\.b)
**	ret
*/
TEST_UNIFORM_Z (inc_b8_s8_general_z0, svint8_t,
		z0 = svadd_n_s8_x (svptrue_b8 (), z0, svcntp_b8 (p0, p1)),
		z0 = svadd_x (svptrue_b8 (), z0, svcntp_b8 (p0, p1)));

/*
** inc_b8_s8_general_z1:
**	cntp	x([0-9]+), p0, p1\.b
**	mov	(z[0-9]+\.b), w\1
**	add	z0\.b, (z1\.b, \2|\2, z1\.b)
**	ret
*/
TEST_UNIFORM_Z (inc_b8_s8_general_z1, svint8_t,
		z0 = svadd_n_s8_x (svptrue_b8 (), z1, svcntp_b8 (p0, p1)),
		z0 = svadd_x (svptrue_b8 (), z1, svcntp_b8 (p0, p1)));

/*
** inc_b8_s8_ptrue_z0:
**	ptrue	(p[0-7])\.b, all
**	cntp	x([0-9]+), \1, p0\.b
**	mov	(z[0-9]+\.b), w\2
**	add	z0\.b, (z0\.b, \3|\3, z0\.b)
**	ret
*/
TEST_UNIFORM_Z (inc_b8_s8_ptrue_z0, svint8_t,
		z0 = svadd_n_s8_x (svptrue_b8 (), z0, svcntp_b8 (svptrue_b8 (), p0)),
		z0 = svadd_x (svptrue_b8 (), z0, svcntp_b8 (svptrue_b8 (), p0)));

/*
** inc_b8_s8_ptrue_z1:
**	ptrue	(p[0-7])\.b, all
**	cntp	x([0-9]+), \1, p0\.b
**	mov	(z[0-9]+\.b), w\2
**	add	z0\.b, (z1\.b, \3|\3, z1\.b)
**	ret
*/
TEST_UNIFORM_Z (inc_b8_s8_ptrue_z1, svint8_t,
		z0 = svadd_n_s8_x (svptrue_b8 (), z1, svcntp_b8 (svptrue_b8 (), p0)),
		z0 = svadd_x (svptrue_b8 (), z1, svcntp_b8 (svptrue_b8 (), p0)));

/*
** dec_b8_s8_general_z0:
**	cntp	x([0-9]+), p0, p1\.b
**	mov	(z[0-9]+\.b), w\1
**	sub	z0\.b, z0\.b, \2
**	ret
*/
TEST_UNIFORM_Z (dec_b8_s8_general_z0, svint8_t,
		z0 = svsub_n_s8_x (svptrue_b8 (), z0, svcntp_b8 (p0, p1)),
		z0 = svsub_x (svptrue_b8 (), z0, svcntp_b8 (p0, p1)));

/*
** dec_b8_s8_general_z1:
**	cntp	x([0-9]+), p0, p1\.b
**	mov	(z[0-9]+\.b), w\1
**	sub	z0\.b, z1\.b, \2
**	ret
*/
TEST_UNIFORM_Z (dec_b8_s8_general_z1, svint8_t,
		z0 = svsub_n_s8_x (svptrue_b8 (), z1, svcntp_b8 (p0, p1)),
		z0 = svsub_x (svptrue_b8 (), z1, svcntp_b8 (p0, p1)));

/*
** dec_b8_s8_ptrue_z0:
**	ptrue	(p[0-7])\.b, all
**	cntp	x([0-9]+), \1, p0\.b
**	mov	(z[0-9]+\.b), w\2
**	sub	z0\.b, z0\.b, \3
**	ret
*/
TEST_UNIFORM_Z (dec_b8_s8_ptrue_z0, svint8_t,
		z0 = svsub_n_s8_x (svptrue_b8 (), z0, svcntp_b8 (svptrue_b8 (), p0)),
		z0 = svsub_x (svptrue_b8 (), z0, svcntp_b8 (svptrue_b8 (), p0)));

/*
** dec_b8_s8_ptrue_z1:
**	ptrue	(p[0-7])\.b, all
**	cntp	x([0-9]+), \1, p0\.b
**	mov	(z[0-9]+\.b), w\2
**	sub	z0\.b, z1\.b, \3
**	ret
*/
TEST_UNIFORM_Z (dec_b8_s8_ptrue_z1, svint8_t,
		z0 = svsub_n_s8_x (svptrue_b8 (), z1, svcntp_b8 (svptrue_b8 (), p0)),
		z0 = svsub_x (svptrue_b8 (), z1, svcntp_b8 (svptrue_b8 (), p0)));
