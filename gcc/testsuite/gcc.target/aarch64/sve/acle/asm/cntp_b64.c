/* { dg-additional-options "-msve-vector-bits=scalable" } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"
#include <stdbool.h>

/*
** cnt_b64_32:
**	cntp	x0, p0, p1\.d
**	ret
*/
TEST_PTEST (cnt_b64_32, uint32_t,
	    x0 = svcntp_b64 (p0, p1));

/*
** cnt_b64_64:
**	cntp	x0, p0, p1\.d
**	ret
*/
TEST_PTEST (cnt_b64_64, uint64_t,
	    x0 = svcntp_b64 (p0, p1));

/*
** inc_b64_32_general_x0:
**	cntp	x([0-9]+), p0, p1\.d
**	add	w0, (w0, w\1|w\1, w0)
**	ret
*/
TEST_PTEST (inc_b64_32_general_x0, uint32_t,
	    x0 += svcntp_b64 (p0, p1));

/*
** inc_b64_32_general_x1:
**	cntp	x([0-9]+), p0, p1\.d
**	add	w0, (w1, w\1|w\1, w1)
**	ret
*/
TEST_PTEST (inc_b64_32_general_x1, uint32_t,
	    x0 = x1 + svcntp_b64 (p0, p1));

/*
** inc_b64_32_ptrue_x0:
**	incp	x0, p1\.d
**	ret
*/
TEST_PTEST (inc_b64_32_ptrue_x0, uint32_t,
	    x0 += svcntp_b64 (svptrue_b64 (), p1));

/*
** inc_b64_32_ptrue_x1:
**	mov	w0, w1
**	incp	x0, p1\.d
**	ret
*/
TEST_PTEST (inc_b64_32_ptrue_x1, uint32_t,
	    x0 = x1 + svcntp_b64 (svptrue_b64 (), p1));

/*
** inc_b64_64_general_x0:
**	cntp	(x[0-9]+), p0, p1\.d
**	add	x0, (x0, \1|\1, x0)
**	ret
*/
TEST_PTEST (inc_b64_64_general_x0, uint64_t,
	    x0 += svcntp_b64 (p0, p1));

/*
** inc_b64_64_general_x1:
**	cntp	(x[0-9]+), p0, p1\.d
**	add	x0, (x1, \1|\1, x1)
**	ret
*/
TEST_PTEST (inc_b64_64_general_x1, uint64_t,
	    x0 = x1 + svcntp_b64 (p0, p1));

/*
** inc_b64_64_ptrue_x0:
**	incp	x0, p1\.d
**	ret
*/
TEST_PTEST (inc_b64_64_ptrue_x0, uint64_t,
	    x0 += svcntp_b64 (svptrue_b64 (), p1));

/*
** inc_b64_64_ptrue_x1:
**	mov	x0, x1
**	incp	x0, p1\.d
**	ret
*/
TEST_PTEST (inc_b64_64_ptrue_x1, uint64_t,
	    x0 = x1 + svcntp_b64 (svptrue_b64 (), p1));

/*
** dec_b64_32_general_x0:
**	cntp	x([0-9]+), p0, p1\.d
**	sub	w0, w0, w\1
**	ret
*/
TEST_PTEST (dec_b64_32_general_x0, uint32_t,
	    x0 -= svcntp_b64 (p0, p1));

/*
** dec_b64_32_general_x1:
**	cntp	x([0-9]+), p0, p1\.d
**	sub	w0, w1, w\1
**	ret
*/
TEST_PTEST (dec_b64_32_general_x1, uint32_t,
	    x0 = x1 - svcntp_b64 (p0, p1));

/*
** dec_b64_32_ptrue_x0:
**	decp	x0, p1\.d
**	ret
*/
TEST_PTEST (dec_b64_32_ptrue_x0, uint32_t,
	    x0 -= svcntp_b64 (svptrue_b64 (), p1));

/*
** dec_b64_32_ptrue_x1:
**	mov	w0, w1
**	decp	x0, p1\.d
**	ret
*/
TEST_PTEST (dec_b64_32_ptrue_x1, uint32_t,
	    x0 = x1 - svcntp_b64 (svptrue_b64 (), p1));

/*
** dec_b64_64_general_x0:
**	cntp	(x[0-9]+), p0, p1\.d
**	sub	x0, x0, \1
**	ret
*/
TEST_PTEST (dec_b64_64_general_x0, uint64_t,
	    x0 -= svcntp_b64 (p0, p1));

/*
** dec_b64_64_general_x1:
**	cntp	(x[0-9]+), p0, p1\.d
**	sub	x0, x1, \1
**	ret
*/
TEST_PTEST (dec_b64_64_general_x1, uint64_t,
	    x0 = x1 - svcntp_b64 (p0, p1));

/*
** dec_b64_64_ptrue_x0:
**	decp	x0, p1\.d
**	ret
*/
TEST_PTEST (dec_b64_64_ptrue_x0, uint64_t,
	    x0 -= svcntp_b64 (svptrue_b64 (), p1));

/*
** dec_b64_64_ptrue_x1:
**	mov	x0, x1
**	decp	x0, p1\.d
**	ret
*/
TEST_PTEST (dec_b64_64_ptrue_x1, uint64_t,
	    x0 = x1 - svcntp_b64 (svptrue_b64 (), p1));

/*
** inc_b64_u64_general_z0:
**	cntp	(x[0-9]+), p0, p1\.d
**	mov	(z[0-9]+\.d), \1
**	add	z0\.d, (z0\.d, \2|\2, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (inc_b64_u64_general_z0, svuint64_t,
		z0 = svadd_n_u64_x (svptrue_b64 (), z0, svcntp_b64 (p0, p1)),
		z0 = svadd_x (svptrue_b64 (), z0, svcntp_b64 (p0, p1)));

/*
** inc_b64_u64_general_z1:
**	cntp	(x[0-9]+), p0, p1\.d
**	mov	(z[0-9]+\.d), \1
**	add	z0\.d, (z1\.d, \2|\2, z1\.d)
**	ret
*/
TEST_UNIFORM_Z (inc_b64_u64_general_z1, svuint64_t,
		z0 = svadd_n_u64_x (svptrue_b64 (), z1, svcntp_b64 (p0, p1)),
		z0 = svadd_x (svptrue_b64 (), z1, svcntp_b64 (p0, p1)));

/*
** inc_b64_u64_ptrue_z0:
**	incp	z0\.d, p0
**	ret
*/
TEST_UNIFORM_Z (inc_b64_u64_ptrue_z0, svuint64_t,
		z0 = svadd_n_u64_x (svptrue_b64 (), z0, svcntp_b64 (svptrue_b64 (), p0)),
		z0 = svadd_x (svptrue_b64 (), z0, svcntp_b64 (svptrue_b64 (), p0)));

/*
** inc_b64_u64_ptrue_z1:
**	movprfx	z0, z1
**	incp	z0\.d, p0
**	ret
*/
TEST_UNIFORM_Z (inc_b64_u64_ptrue_z1, svuint64_t,
		z0 = svadd_n_u64_x (svptrue_b64 (), z1, svcntp_b64 (svptrue_b64 (), p0)),
		z0 = svadd_x (svptrue_b64 (), z1, svcntp_b64 (svptrue_b64 (), p0)));

/*
** dec_b64_u64_general_z0:
**	cntp	(x[0-9]+), p0, p1\.d
**	mov	(z[0-9]+\.d), \1
**	sub	z0\.d, z0\.d, \2
**	ret
*/
TEST_UNIFORM_Z (dec_b64_u64_general_z0, svuint64_t,
		z0 = svsub_n_u64_x (svptrue_b64 (), z0, svcntp_b64 (p0, p1)),
		z0 = svsub_x (svptrue_b64 (), z0, svcntp_b64 (p0, p1)));

/*
** dec_b64_u64_general_z1:
**	cntp	(x[0-9]+), p0, p1\.d
**	mov	(z[0-9]+\.d), \1
**	sub	z0\.d, z1\.d, \2
**	ret
*/
TEST_UNIFORM_Z (dec_b64_u64_general_z1, svuint64_t,
		z0 = svsub_n_u64_x (svptrue_b64 (), z1, svcntp_b64 (p0, p1)),
		z0 = svsub_x (svptrue_b64 (), z1, svcntp_b64 (p0, p1)));

/*
** dec_b64_u64_ptrue_z0:
**	decp	z0\.d, p0
**	ret
*/
TEST_UNIFORM_Z (dec_b64_u64_ptrue_z0, svuint64_t,
		z0 = svsub_n_u64_x (svptrue_b64 (), z0, svcntp_b64 (svptrue_b64 (), p0)),
		z0 = svsub_x (svptrue_b64 (), z0, svcntp_b64 (svptrue_b64 (), p0)));

/*
** dec_b64_u64_ptrue_z1:
**	movprfx	z0, z1
**	decp	z0\.d, p0
**	ret
*/
TEST_UNIFORM_Z (dec_b64_u64_ptrue_z1, svuint64_t,
		z0 = svsub_n_u64_x (svptrue_b64 (), z1, svcntp_b64 (svptrue_b64 (), p0)),
		z0 = svsub_x (svptrue_b64 (), z1, svcntp_b64 (svptrue_b64 (), p0)));
