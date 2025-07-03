// Test that the instructions added by FEAT_CMPBR are emitted
// { dg-do compile }
// { dg-do-if assemble { target aarch64_asm_cmpbr_ok } }
// { dg-options "-march=armv9.5-a+cmpbr -O2" }
// { dg-final { check-function-bodies "**" "*/" "" { target *-*-* } {\.L[0-9]+} } }

#include <stdint.h>

typedef uint8_t u8;
typedef int8_t i8;

typedef uint16_t u16;
typedef int16_t i16;

typedef uint32_t u32;
typedef int32_t i32;

typedef uint64_t u64;
typedef int64_t i64;

int taken();
int not_taken();

#define COMPARE(ty, name, op, rhs)                                             \
  int ty##_x0_##name##_##rhs(ty x0, ty x1) {                                   \
    return __builtin_expect(x0 op rhs, 0) ? taken() : not_taken();             \
  }

#define COMPARE_ALL(unsigned_ty, signed_ty, rhs)                               \
  COMPARE(unsigned_ty, eq, ==, rhs);                                           \
  COMPARE(unsigned_ty, ne, !=, rhs);                                           \
                                                                               \
  COMPARE(unsigned_ty, ult, <, rhs);                                           \
  COMPARE(unsigned_ty, ule, <=, rhs);                                          \
  COMPARE(unsigned_ty, ugt, >, rhs);                                           \
  COMPARE(unsigned_ty, uge, >=, rhs);                                          \
                                                                               \
  COMPARE(signed_ty, slt, <, rhs);                                             \
  COMPARE(signed_ty, sle, <=, rhs);                                            \
  COMPARE(signed_ty, sgt, >, rhs);                                             \
  COMPARE(signed_ty, sge, >=, rhs);

// ==== CBB<cc> (register) ====
COMPARE_ALL(u8, i8, x1);

// ==== CBH<cc> (register) ====
COMPARE_ALL(u16, i16, x1);

// ==== CB<cc> (register) ====
COMPARE_ALL(u32, i32, x1);
COMPARE_ALL(u64, i64, x1);

// ==== CB<cc> (immediate) ====
COMPARE_ALL(u32, i32, 42);
COMPARE_ALL(u64, i64, 42);

// ==== Special cases ====
// Comparisons against the immediate 0 can be done for all types,
// because we can use the wzr/xzr register as one of the operands.
// However, we should prefer to use CBZ/CBNZ or TBZ/TBNZ when possible,
// because they have larger range.
COMPARE_ALL(u8, i8, 0);
COMPARE_ALL(u16, i16, 0);
COMPARE_ALL(u32, i32, 0);
COMPARE_ALL(u64, i64, 0);

// CBB and CBH cannot have immediate operands.
// Instead we have to do a MOV+CB.
COMPARE_ALL(u8, i8, 42);
COMPARE_ALL(u16, i16, 42);

// 64 is out of the range for immediate operands (0 to 63).
// * For 8/16-bit types, use a MOV+CB as above.
// * For 32/64-bit types, use a CMP+B<cc> instead,
//   because B<cc> has a longer range than CB<cc>.
COMPARE_ALL(u8, i8, 64);
COMPARE_ALL(u16, i16, 64);
COMPARE_ALL(u32, i32, 64);
COMPARE_ALL(u64, i64, 64);

// 4098 is out of the range for CMP (0 to 4095, optionally shifted by left by 12
// bits), but it can be materialized in a single MOV.
COMPARE_ALL(u16, i16, 4098);
COMPARE_ALL(u32, i32, 4098);
COMPARE_ALL(u64, i64, 4098);

// If the branch destination is out of range (1KiB), we have to generate an
// extra B instruction (which can handle larger displacements) and branch around
// it

// clang-format off
#define STORE_1()   z = 0;
#define STORE_2()   STORE_1()   STORE_1()
#define STORE_4()   STORE_2()   STORE_2()
#define STORE_8()   STORE_4()   STORE_4()
#define STORE_16()  STORE_8()   STORE_8()
#define STORE_32()  STORE_16()  STORE_16()
#define STORE_64()  STORE_32()  STORE_32()
#define STORE_128() STORE_64()  STORE_64()
#define STORE_256() STORE_128() STORE_128()
// clang-format on

#define FAR_BRANCH(ty, rhs)                                                    \
  int far_branch_##ty##_x0_eq_##rhs(ty x0, ty x1) {                            \
    volatile int z = 0;                                                        \
    if (__builtin_expect(x0 == rhs, 1)) {                                      \
      STORE_256();                                                             \
    }                                                                          \
    return taken();                                                            \
  }

FAR_BRANCH(u8, x1);
FAR_BRANCH(u16, x1);
FAR_BRANCH(u32, x1);
FAR_BRANCH(u64, x1);

FAR_BRANCH(u8, 42);
FAR_BRANCH(u16, 42);
FAR_BRANCH(u32, 42);
FAR_BRANCH(u64, 42);

/*
** u8_x0_eq_x1:
**	and	(w[0-9]+), w1, 255
**	cmp	\1, w0, uxtb
**	beq	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u8_x0_ne_x1:
**	and	(w[0-9]+), w1, 255
**	cmp	\1, w0, uxtb
**	bne	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u8_x0_ult_x1:
**	and	(w[0-9]+), w1, 255
**	cmp	\1, w0, uxtb
**	bhi	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u8_x0_ule_x1:
**	and	(w[0-9]+), w1, 255
**	cmp	\1, w0, uxtb
**	bcs	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u8_x0_ugt_x1:
**	and	(w[0-9]+), w1, 255
**	cmp	\1, w0, uxtb
**	bcc	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u8_x0_uge_x1:
**	and	(w[0-9]+), w1, 255
**	cmp	\1, w0, uxtb
**	bls	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i8_x0_slt_x1:
**	sxtb	(w[0-9]+), w1
**	cmp	\1, w0, sxtb
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i8_x0_sle_x1:
**	sxtb	(w[0-9]+), w1
**	cmp	\1, w0, sxtb
**	bge	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i8_x0_sgt_x1:
**	sxtb	(w[0-9]+), w1
**	cmp	\1, w0, sxtb
**	blt	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i8_x0_sge_x1:
**	sxtb	(w[0-9]+), w1
**	cmp	\1, w0, sxtb
**	ble	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_eq_x1:
**	and	(w[0-9]+), w1, 65535
**	cmp	\1, w0, uxth
**	beq	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ne_x1:
**	and	(w[0-9]+), w1, 65535
**	cmp	\1, w0, uxth
**	bne	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ult_x1:
**	and	(w[0-9]+), w1, 65535
**	cmp	\1, w0, uxth
**	bhi	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ule_x1:
**	and	(w[0-9]+), w1, 65535
**	cmp	\1, w0, uxth
**	bcs	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ugt_x1:
**	and	(w[0-9]+), w1, 65535
**	cmp	\1, w0, uxth
**	bcc	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_uge_x1:
**	and	(w[0-9]+), w1, 65535
**	cmp	\1, w0, uxth
**	bls	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_slt_x1:
**	sxth	(w[0-9]+), w1
**	cmp	\1, w0, sxth
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_sle_x1:
**	sxth	(w[0-9]+), w1
**	cmp	\1, w0, sxth
**	bge	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_sgt_x1:
**	sxth	(w[0-9]+), w1
**	cmp	\1, w0, sxth
**	blt	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_sge_x1:
**	sxth	(w[0-9]+), w1
**	cmp	\1, w0, sxth
**	ble	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u32_x0_eq_x1:
**	cmp	w0, w1
**	beq	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_ne_x1:
**	cmp	w0, w1
**	bne	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_ult_x1:
**	cmp	w0, w1
**	bcc	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_ule_x1:
**	cmp	w0, w1
**	bls	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_ugt_x1:
**	cmp	w0, w1
**	bhi	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_uge_x1:
**	cmp	w0, w1
**	bcs	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i32_x0_slt_x1:
**	cmp	w0, w1
**	blt	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i32_x0_sle_x1:
**	cmp	w0, w1
**	ble	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i32_x0_sgt_x1:
**	cmp	w0, w1
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i32_x0_sge_x1:
**	cmp	w0, w1
**	bge	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_eq_x1:
**	cmp	x0, x1
**	beq	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_ne_x1:
**	cmp	x0, x1
**	bne	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_ult_x1:
**	cmp	x0, x1
**	bcc	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_ule_x1:
**	cmp	x0, x1
**	bls	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_ugt_x1:
**	cmp	x0, x1
**	bhi	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_uge_x1:
**	cmp	x0, x1
**	bcs	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i64_x0_slt_x1:
**	cmp	x0, x1
**	blt	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i64_x0_sle_x1:
**	cmp	x0, x1
**	ble	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i64_x0_sgt_x1:
**	cmp	x0, x1
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i64_x0_sge_x1:
**	cmp	x0, x1
**	bge	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_eq_42:
**	cmp	w0, 42
**	beq	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_ne_42:
**	cmp	w0, 42
**	bne	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_ult_42:
**	cmp	w0, 41
**	bls	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_ule_42:
**	cmp	w0, 42
**	bls	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_ugt_42:
**	cmp	w0, 42
**	bhi	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_uge_42:
**	cmp	w0, 41
**	bhi	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i32_x0_slt_42:
**	cmp	w0, 41
**	ble	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i32_x0_sle_42:
**	cmp	w0, 42
**	ble	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i32_x0_sgt_42:
**	cmp	w0, 42
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i32_x0_sge_42:
**	cmp	w0, 41
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_eq_42:
**	cmp	x0, 42
**	beq	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_ne_42:
**	cmp	x0, 42
**	bne	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_ult_42:
**	cmp	x0, 41
**	bls	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_ule_42:
**	cmp	x0, 42
**	bls	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_ugt_42:
**	cmp	x0, 42
**	bhi	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_uge_42:
**	cmp	x0, 41
**	bhi	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i64_x0_slt_42:
**	cmp	x0, 41
**	ble	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i64_x0_sle_42:
**	cmp	x0, 42
**	ble	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i64_x0_sgt_42:
**	cmp	x0, 42
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i64_x0_sge_42:
**	cmp	x0, 41
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u8_x0_eq_0:
**	tst	w0, 255
**	beq	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u8_x0_ne_0:
**	tst	w0, 255
**	bne	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u8_x0_ult_0:
**	b	not_taken
*/

/*
** u8_x0_ule_0:
**	tst	w0, 255
**	beq	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u8_x0_ugt_0:
**	tst	w0, 255
**	bne	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u8_x0_uge_0:
**	b	taken
*/

/*
** i8_x0_slt_0:
**	tbnz	w0, 7, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i8_x0_sle_0:
**	sxtb	(w[0-9]+), w0
**	cmp	\1, 0
**	ble	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i8_x0_sgt_0:
**	sxtb	(w[0-9]+), w0
**	cmp	\1, 0
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i8_x0_sge_0:
**	tbz	w0, 7, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u16_x0_eq_0:
**	tst	w0, 65535
**	beq	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u16_x0_ne_0:
**	tst	w0, 65535
**	bne	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u16_x0_ult_0:
**	b	not_taken
*/

/*
** u16_x0_ule_0:
**	tst	w0, 65535
**	beq	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u16_x0_ugt_0:
**	tst	w0, 65535
**	bne	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u16_x0_uge_0:
**	b	taken
*/

/*
** i16_x0_slt_0:
**	tbnz	w0, 15, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i16_x0_sle_0:
**	sxth	(w[0-9]+), w0
**	cmp	\1, 0
**	ble	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_sgt_0:
**	sxth	(w[0-9]+), w0
**	cmp	\1, 0
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_sge_0:
**	tbz	w0, 15, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_eq_0:
**	cbz	w0, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_ne_0:
**	cbnz	w0, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_ult_0:
**	b	not_taken
*/

/*
** u32_x0_ule_0:
**	cbz	w0, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_ugt_0:
**	cbnz	w0, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_uge_0:
**	b	taken
*/

/*
** i32_x0_slt_0:
**	tbnz	w0, #31, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i32_x0_sle_0:
**	cmp	w0, 0
**	ble	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i32_x0_sgt_0:
**	cmp	w0, 0
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i32_x0_sge_0:
**	tbz	w0, #31, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_eq_0:
**	cbz	x0, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_ne_0:
**	cbnz	x0, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_ult_0:
**	b	not_taken
*/

/*
** u64_x0_ule_0:
**	cbz	x0, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_ugt_0:
**	cbnz	x0, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_uge_0:
**	b	taken
*/

/*
** i64_x0_slt_0:
**	tbnz	x0, #63, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i64_x0_sle_0:
**	cmp	x0, 0
**	ble	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i64_x0_sgt_0:
**	cmp	x0, 0
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i64_x0_sge_0:
**	tbz	x0, #63, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u8_x0_eq_42:
**	and	(w[0-9]+), w0, 255
**	cmp	\1, 42
**	beq	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u8_x0_ne_42:
**	and	(w[0-9]+), w0, 255
**	cmp	\1, 42
**	bne	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u8_x0_ult_42:
**	and	(w[0-9]+), w0, 255
**	cmp	\1, 41
**	bls	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u8_x0_ule_42:
**	and	(w[0-9]+), w0, 255
**	cmp	\1, 42
**	bls	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u8_x0_ugt_42:
**	and	(w[0-9]+), w0, 255
**	cmp	\1, 42
**	bhi	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u8_x0_uge_42:
**	and	(w[0-9]+), w0, 255
**	cmp	\1, 41
**	bhi	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i8_x0_slt_42:
**	sxtb	(w[0-9]+), w0
**	cmp	\1, 41
**	ble	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i8_x0_sle_42:
**	sxtb	(w[0-9]+), w0
**	cmp	\1, 42
**	ble	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i8_x0_sgt_42:
**	sxtb	(w[0-9]+), w0
**	cmp	\1, 42
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i8_x0_sge_42:
**	sxtb	(w[0-9]+), w0
**	cmp	\1, 41
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_eq_42:
**	and	(w[0-9]+), w0, 65535
**	cmp	\1, 42
**	beq	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ne_42:
**	and	(w[0-9]+), w0, 65535
**	cmp	\1, 42
**	bne	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ult_42:
**	and	(w[0-9]+), w0, 65535
**	cmp	\1, 41
**	bls	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ule_42:
**	and	(w[0-9]+), w0, 65535
**	cmp	\1, 42
**	bls	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ugt_42:
**	and	(w[0-9]+), w0, 65535
**	cmp	\1, 42
**	bhi	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_uge_42:
**	and	(w[0-9]+), w0, 65535
**	cmp	\1, 41
**	bhi	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_slt_42:
**	sxth	(w[0-9]+), w0
**	cmp	\1, 41
**	ble	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_sle_42:
**	sxth	(w[0-9]+), w0
**	cmp	\1, 42
**	ble	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_sgt_42:
**	sxth	(w[0-9]+), w0
**	cmp	\1, 42
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_sge_42:
**	sxth	(w[0-9]+), w0
**	cmp	\1, 41
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u8_x0_eq_64:
**	and	(w[0-9]+), w0, 255
**	cmp	\1, 64
**	beq	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u8_x0_ne_64:
**	and	(w[0-9]+), w0, 255
**	cmp	\1, 64
**	bne	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u8_x0_ult_64:
**	and	(w[0-9]+), w0, 255
**	cmp	\1, 63
**	bls	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u8_x0_ule_64:
**	and	(w[0-9]+), w0, 255
**	cmp	\1, 64
**	bls	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u8_x0_ugt_64:
**	and	(w[0-9]+), w0, 255
**	cmp	\1, 64
**	bhi	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u8_x0_uge_64:
**	and	(w[0-9]+), w0, 255
**	cmp	\1, 63
**	bhi	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i8_x0_slt_64:
**	sxtb	(w[0-9]+), w0
**	cmp	\1, 63
**	ble	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i8_x0_sle_64:
**	sxtb	(w[0-9]+), w0
**	cmp	\1, 64
**	ble	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i8_x0_sgt_64:
**	sxtb	(w[0-9]+), w0
**	cmp	\1, 64
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i8_x0_sge_64:
**	sxtb	(w[0-9]+), w0
**	cmp	\1, 63
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_eq_64:
**	and	(w[0-9]+), w0, 65535
**	cmp	\1, 64
**	beq	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ne_64:
**	and	(w[0-9]+), w0, 65535
**	cmp	\1, 64
**	bne	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ult_64:
**	and	(w[0-9]+), w0, 65535
**	cmp	\1, 63
**	bls	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ule_64:
**	and	(w[0-9]+), w0, 65535
**	cmp	\1, 64
**	bls	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ugt_64:
**	and	(w[0-9]+), w0, 65535
**	cmp	\1, 64
**	bhi	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_uge_64:
**	and	(w[0-9]+), w0, 65535
**	cmp	\1, 63
**	bhi	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_slt_64:
**	sxth	(w[0-9]+), w0
**	cmp	\1, 63
**	ble	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_sle_64:
**	sxth	(w[0-9]+), w0
**	cmp	\1, 64
**	ble	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_sgt_64:
**	sxth	(w[0-9]+), w0
**	cmp	\1, 64
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_sge_64:
**	sxth	(w[0-9]+), w0
**	cmp	\1, 63
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u32_x0_eq_64:
**	cmp	w0, 64
**	beq	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_ne_64:
**	cmp	w0, 64
**	bne	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_ult_64:
**	cmp	w0, 63
**	bls	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_ule_64:
**	cmp	w0, 64
**	bls	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_ugt_64:
**	cmp	w0, 64
**	bhi	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_uge_64:
**	cmp	w0, 63
**	bhi	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i32_x0_slt_64:
**	cmp	w0, 63
**	ble	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i32_x0_sle_64:
**	cmp	w0, 64
**	ble	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i32_x0_sgt_64:
**	cmp	w0, 64
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i32_x0_sge_64:
**	cmp	w0, 63
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_eq_64:
**	cmp	x0, 64
**	beq	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_ne_64:
**	cmp	x0, 64
**	bne	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_ult_64:
**	cmp	x0, 63
**	bls	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_ule_64:
**	cmp	x0, 64
**	bls	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_ugt_64:
**	cmp	x0, 64
**	bhi	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_uge_64:
**	cmp	x0, 63
**	bhi	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i64_x0_slt_64:
**	cmp	x0, 63
**	ble	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i64_x0_sle_64:
**	cmp	x0, 64
**	ble	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i64_x0_sgt_64:
**	cmp	x0, 64
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i64_x0_sge_64:
**	cmp	x0, 63
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u16_x0_eq_4098:
**	mov	(w[0-9]+), 4098
**	cmp	\1, w0, uxth
**	beq	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ne_4098:
**	mov	(w[0-9]+), 4098
**	cmp	\1, w0, uxth
**	bne	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ult_4098:
**	mov	(w[0-9]+), 4097
**	cmp	\1, w0, uxth
**	bcs	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ule_4098:
**	mov	(w[0-9]+), 4098
**	cmp	\1, w0, uxth
**	bcs	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ugt_4098:
**	mov	(w[0-9]+), 4098
**	cmp	\1, w0, uxth
**	bcc	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_uge_4098:
**	mov	(w[0-9]+), 4097
**	cmp	\1, w0, uxth
**	bcc	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_slt_4098:
**	mov	(w[0-9]+), 4097
**	cmp	\1, w0, sxth
**	bge	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_sle_4098:
**	mov	(w[0-9]+), 4098
**	cmp	\1, w0, sxth
**	bge	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_sgt_4098:
**	mov	(w[0-9]+), 4098
**	cmp	\1, w0, sxth
**	blt	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_sge_4098:
**	mov	(w[0-9]+), 4097
**	cmp	\1, w0, sxth
**	blt	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u32_x0_eq_4098:
**	mov	(w[0-9]+), 4098
**	cmp	w0, \1
**	beq	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u32_x0_ne_4098:
**	mov	(w[0-9]+), 4098
**	cmp	w0, \1
**	bne	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u32_x0_ult_4098:
**	mov	(w[0-9]+), 4097
**	cmp	w0, \1
**	bls	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u32_x0_ule_4098:
**	mov	(w[0-9]+), 4098
**	cmp	w0, \1
**	bls	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u32_x0_ugt_4098:
**	mov	(w[0-9]+), 4098
**	cmp	w0, \1
**	bhi	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u32_x0_uge_4098:
**	mov	(w[0-9]+), 4097
**	cmp	w0, \1
**	bhi	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i32_x0_slt_4098:
**	mov	(w[0-9]+), 4097
**	cmp	w0, \1
**	ble	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i32_x0_sle_4098:
**	mov	(w[0-9]+), 4098
**	cmp	w0, \1
**	ble	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i32_x0_sgt_4098:
**	mov	(w[0-9]+), 4098
**	cmp	w0, \1
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i32_x0_sge_4098:
**	mov	(w[0-9]+), 4097
**	cmp	w0, \1
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u64_x0_eq_4098:
**	mov	(x[0-9]+), 4098
**	cmp	x0, \1
**	beq	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u64_x0_ne_4098:
**	mov	(x[0-9]+), 4098
**	cmp	x0, \1
**	bne	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u64_x0_ult_4098:
**	mov	(x[0-9]+), 4097
**	cmp	x0, \1
**	bls	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u64_x0_ule_4098:
**	mov	(x[0-9]+), 4098
**	cmp	x0, \1
**	bls	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u64_x0_ugt_4098:
**	mov	(x[0-9]+), 4098
**	cmp	x0, \1
**	bhi	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u64_x0_uge_4098:
**	mov	(x[0-9]+), 4097
**	cmp	x0, \1
**	bhi	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i64_x0_slt_4098:
**	mov	(x[0-9]+), 4097
**	cmp	x0, \1
**	ble	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i64_x0_sle_4098:
**	mov	(x[0-9]+), 4098
**	cmp	x0, \1
**	ble	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i64_x0_sgt_4098:
**	mov	(x[0-9]+), 4098
**	cmp	x0, \1
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i64_x0_sge_4098:
**	mov	(x[0-9]+), 4097
**	cmp	x0, \1
**	bgt	.L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** far_branch_u8_x0_eq_x1:
**	sub	sp, sp, #16
**	and	w([0-9]+), w1, 255
**	str	wzr, \[sp, 12\]
**	cmp	w\1, w0, uxtb
**	bne	.L([0-9]+)
**	str	wzr, \[sp, 12\]
**	...
**	str	wzr, \[sp, 12\]
** .L\2:
**	add	sp, sp, 16
**	b	taken
*/
/*
** far_branch_u16_x0_eq_x1:
**	sub	sp, sp, #16
**	and	w([0-9]+), w1, 65535
**	str	wzr, \[sp, 12\]
**	cmp	w\1, w0, uxth
**	bne	.L([0-9]+)
**	str	wzr, \[sp, 12\]
**	...
**	str	wzr, \[sp, 12\]
** .L\2:
**	add	sp, sp, 16
**	b	taken
*/

/*
** far_branch_u32_x0_eq_x1:
**	sub	sp, sp, #16
**	str	wzr, \[sp, 12\]
**	cmp	w0|w1, w1|w0
**	bne	.L([0-9]+)
**	str	wzr, \[sp, 12\]
**	...
**	str	wzr, \[sp, 12\]
** .L\1:
**	add	sp, sp, 16
**	b	taken
*/

/*
** far_branch_u64_x0_eq_x1:
**	sub	sp, sp, #16
**	str	wzr, \[sp, 12\]
**	cmp	x0|x1, x1|x0
**	bne	.L([0-9]+)
**	str	wzr, \[sp, 12\]
**	...
**	str	wzr, \[sp, 12\]
** .L\1:
**	add	sp, sp, 16
**	b	taken
*/

/*
** far_branch_u8_x0_eq_42:
**	sub	sp, sp, #16
**	and	w([0-9]+), w0, 255
**	str	wzr, \[sp, 12\]
**	cmp	w\1, 42
**	bne	.L([0-9]+)
**	str	wzr, \[sp, 12\]
**	...
**	str	wzr, \[sp, 12\]
** .L\2:
**	add	sp, sp, 16
**	b	taken
*/

/*
** far_branch_u16_x0_eq_42:
**	sub	sp, sp, #16
**	and	w([0-9]+), w0, 65535
**	str	wzr, \[sp, 12\]
**	cmp	w\1, 42
**	bne	.L([0-9]+)
**	str	wzr, \[sp, 12\]
**	...
**	str	wzr, \[sp, 12\]
** .L\2:
**	add	sp, sp, 16
**	b	taken
*/

/*
** far_branch_u32_x0_eq_42:
**	sub	sp, sp, #16
**	str	wzr, \[sp, 12\]
**	cmp	w0, 42
**	bne	.L([0-9]+)
**	str	wzr, \[sp, 12\]
**	...
**	str	wzr, \[sp, 12\]
** .L\1:
**	add	sp, sp, 16
**	b	taken
*/

/*
** far_branch_u64_x0_eq_42:
**	sub	sp, sp, #16
**	str	wzr, \[sp, 12\]
**	cmp	x0, 42
**	bne	.L([0-9]+)
**	str	wzr, \[sp, 12\]
**	...
**	str	wzr, \[sp, 12\]
** .L\1:
**	add	sp, sp, 16
**	b	taken
*/
