/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

#pragma GCC target "+sme-i16i64"

/*
** addva_za64_s64_0_p0_p1_z0:
**	addva	za0\.d, p0/m, p1/m, z0\.d
**	ret
*/
TEST_UNIFORM_ZA (addva_za64_s64_0_p0_p1_z0, svint64_t,
		 svaddva_za64_s64_m (0, p0, p1, z0),
		 svaddva_za64_m (0, p0, p1, z0))

/*
** addva_za64_s64_0_p1_p0_z1:
**	addva	za0\.d, p1/m, p0/m, z1\.d
**	ret
*/
TEST_UNIFORM_ZA (addva_za64_s64_0_p1_p0_z1, svint64_t,
		 svaddva_za64_s64_m (0, p1, p0, z1),
		 svaddva_za64_m (0, p1, p0, z1))

/*
** addva_za64_s64_1_p0_p1_z0:
**	addva	za1\.d, p0/m, p1/m, z0\.d
**	ret
*/
TEST_UNIFORM_ZA (addva_za64_s64_1_p0_p1_z0, svint64_t,
		 svaddva_za64_s64_m (1, p0, p1, z0),
		 svaddva_za64_m (1, p0, p1, z0))

/*
** addva_za64_s64_7_p0_p1_z0:
**	addva	za7\.d, p0/m, p1/m, z0\.d
**	ret
*/
TEST_UNIFORM_ZA (addva_za64_s64_7_p0_p1_z0, svint64_t,
		 svaddva_za64_s64_m (7, p0, p1, z0),
		 svaddva_za64_m (7, p0, p1, z0))

/*
** addva_za64_u64_0_p0_p1_z0:
**	addva	za0\.d, p0/m, p1/m, z0\.d
**	ret
*/
TEST_UNIFORM_ZA (addva_za64_u64_0_p0_p1_z0, svuint64_t,
		 svaddva_za64_u64_m (0, p0, p1, z0),
		 svaddva_za64_m (0, p0, p1, z0))
