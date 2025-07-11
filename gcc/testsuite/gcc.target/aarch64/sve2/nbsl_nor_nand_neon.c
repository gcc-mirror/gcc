/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_neon.h>

#define NAND(x, y)  (~((x) & (y)))
#define NOR(x, y)   (~((x) | (y)))

/*
** nand_d:
** 	nbsl	z0.d, z0.d, z1.d, z1.d
** 	ret
*/
uint32x2_t nand_d(uint32x2_t a, uint32x2_t b) { return NAND(a, b); }

/*
** nand_d_mp:
** 	movprfx	z0, z1
** 	nbsl	z0.d, z0.d, z2.d, z2.d
** 	ret
*/
uint32x2_t nand_d_mp(uint32x2_t c, uint32x2_t a, uint32x2_t b) { return NAND(a, b); }

/*
** nor_d:
** 	nbsl	z0.d, z0.d, z1.d, z0.d
** 	ret
*/
uint32x2_t nor_d(uint32x2_t a, uint32x2_t b) { return NOR(a, b); }

/*
** nor_d_mp:
** 	movprfx	z0, z1
** 	nbsl	z0.d, z0.d, z2.d, z1.d
** 	ret
*/
uint32x2_t nor_d_mp(uint32x2_t c, uint32x2_t a, uint32x2_t b) { return NOR(a, b); }

/*
** nand_q:
** 	nbsl	z0.d, z0.d, z1.d, z1.d
** 	ret
*/
uint64x2_t nand_q(uint64x2_t a, uint64x2_t b) { return NAND(a, b); }

/*
** nand_q_mp:
** 	movprfx	z0, z1
** 	nbsl	z0.d, z0.d, z2.d, z2.d
** 	ret
*/
uint32x4_t nand_q_mp(uint32x4_t c, uint32x4_t a, uint32x4_t b) { return NAND(a, b); }

/*
** nor_q:
** 	nbsl	z0.d, z0.d, z1.d, z0.d
** 	ret
*/
uint64x2_t nor_q(uint64x2_t a, uint64x2_t b) { return NOR(a, b); }

/*
** nor_q_mp:
** 	movprfx	z0, z1
** 	nbsl	z0.d, z0.d, z2.d, z1.d
** 	ret
*/
uint32x4_t nor_q_mp(uint32x4_t c, uint32x4_t a, uint32x4_t b) { return NOR(a, b); }

