// Test that the instructions added by FEAT_CMPBR are emitted
// { dg-do compile }
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
**	cbbeq	(?:w1, w0|w0, w1), .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u8_x0_ne_x1:
**	cbbne	(?:w1, w0|w0, w1), .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u8_x0_ult_x1:
**	(?:cbbhi	w1, w0|cbblo	w0, w1), .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u8_x0_ule_x1:
**	(?:cbbhs	w1, w0|cbbls	w0, w1), .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u8_x0_ugt_x1:
**	(?:cbblo	w1, w0|cbbhi	w0, w1), .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u8_x0_uge_x1:
**	(?:cbbls	w1, w0|cbbhs	w0, w1), .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i8_x0_slt_x1:
**	(?:cbbgt	w1, w0|cbblt	w0, w1), .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i8_x0_sle_x1:
**	(?:cbbge	w1, w0|cbble	w0, w1), .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i8_x0_sgt_x1:
**	(?:cbblt	w1, w0|cbbgt	w0, w1), .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i8_x0_sge_x1:
**	(?:cbble	w1, w0|cbbge	w0, w1), .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u16_x0_eq_x1:
**	cbheq	(?:w1, w0|w0, w1), .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u16_x0_ne_x1:
**	cbhne	(?:w1, w0|w0, w1), .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u16_x0_ult_x1:
**	(?:cbhhi	w1, w0|cbhlo	w0, w1), .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u16_x0_ule_x1:
**	(?:cbhhs	w1, w0|cbhls	w0, w1), .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u16_x0_ugt_x1:
**	(?:cbhlo	w1, w0|cbhhi	w0, w1), .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u16_x0_uge_x1:
**	(?:cbhls	w1, w0|cbhhs	w0, w1), .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i16_x0_slt_x1:
**	(?:cbhgt	w1, w0|cbhlt	w0, w1), .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i16_x0_sle_x1:
**	(?:cbhge	w1, w0|cbhle	w0, w1), .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i16_x0_sgt_x1:
**	(?:cbhlt	w1, w0|cbhgt	w0, w1), .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i16_x0_sge_x1:
**	(?:cbhle	w1, w0|cbhge	w0, w1), .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_eq_x1:
**	cbeq	w0, w1, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_ne_x1:
**	cbne	w0, w1, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_ult_x1:
**	cblo	w0, w1, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_ule_x1:
**	cbls	w0, w1, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_ugt_x1:
**	cbhi	w0, w1, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_uge_x1:
**	cbhs	w0, w1, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i32_x0_slt_x1:
**	cblt	w0, w1, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i32_x0_sle_x1:
**	cble	w0, w1, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i32_x0_sgt_x1:
**	cbgt	w0, w1, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i32_x0_sge_x1:
**	cbge	w0, w1, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_eq_x1:
**	cbeq	x0, x1, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_ne_x1:
**	cbne	x0, x1, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_ult_x1:
**	cblo	x0, x1, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_ule_x1:
**	cbls	x0, x1, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_ugt_x1:
**	cbhi	x0, x1, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_uge_x1:
**	cbhs	x0, x1, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i64_x0_slt_x1:
**	cblt	x0, x1, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i64_x0_sle_x1:
**	cble	x0, x1, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i64_x0_sgt_x1:
**	cbgt	x0, x1, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i64_x0_sge_x1:
**	cbge	x0, x1, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_eq_42:
**	cbeq	w0, 42, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_ne_42:
**	cbne	w0, 42, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_ult_42:
**	cbls	w0, 41, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_ule_42:
**	cbls	w0, 42, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_ugt_42:
**	cbhi	w0, 42, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u32_x0_uge_42:
**	cbhi	w0, 41, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i32_x0_slt_42:
**	cble	w0, 41, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i32_x0_sle_42:
**	cble	w0, 42, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i32_x0_sgt_42:
**	cbgt	w0, 42, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i32_x0_sge_42:
**	cbgt	w0, 41, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_eq_42:
**	cbeq	x0, 42, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_ne_42:
**	cbne	x0, 42, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_ult_42:
**	cbls	x0, 41, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_ule_42:
**	cbls	x0, 42, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_ugt_42:
**	cbhi	x0, 42, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u64_x0_uge_42:
**	cbhi	x0, 41, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i64_x0_slt_42:
**	cble	x0, 41, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i64_x0_sle_42:
**	cble	x0, 42, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i64_x0_sgt_42:
**	cbgt	x0, 42, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i64_x0_sge_42:
**	cbgt	x0, 41, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u8_x0_eq_0:
**	cbbeq	w0, wzr, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u8_x0_ne_0:
**	cbbne	w0, wzr, .L([0-9]+)
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
**	cbbeq	w0, wzr, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u8_x0_ugt_0:
**	cbbne	w0, wzr, .L([0-9]+)
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
**	tbnz	w0, #7, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i8_x0_sle_0:
**	cbble	w0, wzr, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i8_x0_sgt_0:
**	cbbgt	w0, wzr, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i8_x0_sge_0:
**	tbz	w0, #7, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u16_x0_eq_0:
**	cbheq	w0, wzr, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u16_x0_ne_0:
**	cbhne	w0, wzr, .L([0-9]+)
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
**	cbheq	w0, wzr, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** u16_x0_ugt_0:
**	cbhne	w0, wzr, .L([0-9]+)
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
**	tbnz	w0, #15, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i16_x0_sle_0:
**	cbhle	w0, wzr, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i16_x0_sgt_0:
**	cbhgt	w0, wzr, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i16_x0_sge_0:
**	tbz	w0, #15, .L([0-9]+)
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
**	cble	w0, wzr, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i32_x0_sgt_0:
**	cbgt	w0, wzr, .L([0-9]+)
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
**	cble	x0, xzr, .L([0-9]+)
**	b	not_taken
** .L\1:
**	b	taken
*/

/*
** i64_x0_sgt_0:
**	cbgt	x0, xzr, .L([0-9]+)
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
**	mov	w([0-9]+), 42
**	cbbeq	w0, w\1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u8_x0_ne_42:
**	mov	(w[0-9]+), 42
**	cbbne	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u8_x0_ult_42:
**	mov	(w[0-9]+), 41
**	cbbls	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u8_x0_ule_42:
**	mov	(w[0-9]+), 42
**	cbbls	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u8_x0_ugt_42:
**	mov	(w[0-9]+), 42
**	cbbhi	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u8_x0_uge_42:
**	mov	(w[0-9]+), 41
**	cbbhi	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i8_x0_slt_42:
**	mov	(w[0-9]+), 41
**	cbble	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i8_x0_sle_42:
**	mov	(w[0-9]+), 42
**	cbble	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i8_x0_sgt_42:
**	mov	(w[0-9]+), 42
**	cbbgt	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i8_x0_sge_42:
**	mov	(w[0-9]+), 41
**	cbbgt	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_eq_42:
**	mov	w([0-9]+), 42
**	cbheq	w0, w\1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ne_42:
**	mov	(w[0-9]+), 42
**	cbhne	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ult_42:
**	mov	(w[0-9]+), 41
**	cbhls	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ule_42:
**	mov	(w[0-9]+), 42
**	cbhls	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ugt_42:
**	mov	(w[0-9]+), 42
**	cbhhi	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_uge_42:
**	mov	(w[0-9]+), 41
**	cbhhi	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_slt_42:
**	mov	(w[0-9]+), 41
**	cbhle	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_sle_42:
**	mov	(w[0-9]+), 42
**	cbhle	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_sgt_42:
**	mov	(w[0-9]+), 42
**	cbhgt	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_sge_42:
**	mov	(w[0-9]+), 41
**	cbhgt	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u8_x0_eq_64:
**	mov	w([0-9]+), 64
**	cbbeq	w0, w\1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u8_x0_ne_64:
**	mov	(w[0-9]+), 64
**	cbbne	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u8_x0_ult_64:
**	mov	(w[0-9]+), 63
**	cbbls	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u8_x0_ule_64:
**	mov	(w[0-9]+), 64
**	cbbls	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u8_x0_ugt_64:
**	mov	(w[0-9]+), 64
**	cbbhi	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u8_x0_uge_64:
**	mov	(w[0-9]+), 63
**	cbbhi	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i8_x0_slt_64:
**	mov	(w[0-9]+), 63
**	cbble	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i8_x0_sle_64:
**	mov	(w[0-9]+), 64
**	cbble	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i8_x0_sgt_64:
**	mov	(w[0-9]+), 64
**	cbbgt	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i8_x0_sge_64:
**	mov	(w[0-9]+), 63
**	cbbgt	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_eq_64:
**	mov	w([0-9]+), 64
**	cbheq	w0, w\1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ne_64:
**	mov	(w[0-9]+), 64
**	cbhne	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ult_64:
**	mov	(w[0-9]+), 63
**	cbhls	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ule_64:
**	mov	(w[0-9]+), 64
**	cbhls	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ugt_64:
**	mov	(w[0-9]+), 64
**	cbhhi	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_uge_64:
**	mov	(w[0-9]+), 63
**	cbhhi	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_slt_64:
**	mov	(w[0-9]+), 63
**	cbhle	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_sle_64:
**	mov	(w[0-9]+), 64
**	cbhle	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_sgt_64:
**	mov	(w[0-9]+), 64
**	cbhgt	w0, w1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_sge_64:
**	mov	(w[0-9]+), 63
**	cbhgt	w0, w1, .L([0-9]+)
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
** u32_x0_ult_64: { xfail *-*-* }
**	cbhi	w0, 63, .L([0-9]+)
**	b	taken
** .L\1:
**	b	not_taken
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
** i32_x0_slt_64: { xfail *-*-* }
**	cbgt	w0, 63, .L([0-9]+)
**	b	taken
** .L\1:
**	b	not_taken
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
** u64_x0_ult_64: { xfail *-*-* }
**	cbhi	x0, 63, .L([0-9]+)
**	b	taken
** .L\1:
**	b	not_taken
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
** i64_x0_slt_64: { xfail *-*-* }
**	cbgt	x0, 63, .L([0-9]+)
**	b	taken
** .L\1:
**	b	not_taken
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
**	mov	w([0-9]+), 4098
**	cbheq	w0, w\1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ne_4098:
**	mov	(w[0-9]+), 4098
**	cbhne	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ult_4098:
**	mov	(w[0-9]+), 4097
**	cbhls	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ule_4098:
**	mov	(w[0-9]+), 4098
**	cbhls	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_ugt_4098:
**	mov	(w[0-9]+), 4098
**	cbhhi	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u16_x0_uge_4098:
**	mov	(w[0-9]+), 4097
**	cbhhi	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_slt_4098:
**	mov	(w[0-9]+), 4097
**	cbhle	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_sle_4098:
**	mov	(w[0-9]+), 4098
**	cbhle	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_sgt_4098:
**	mov	(w[0-9]+), 4098
**	cbhgt	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i16_x0_sge_4098:
**	mov	(w[0-9]+), 4097
**	cbhgt	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u32_x0_eq_4098:
**	mov	w([0-9]+), 4098
**	cbeq	w0, w\1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u32_x0_ne_4098:
**	mov	(w[0-9]+), 4098
**	cbne	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u32_x0_ult_4098:
**	mov	(w[0-9]+), 4097
**	cbls	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u32_x0_ule_4098:
**	mov	(w[0-9]+), 4098
**	cbls	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u32_x0_ugt_4098:
**	mov	(w[0-9]+), 4098
**	cbhi	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u32_x0_uge_4098:
**	mov	(w[0-9]+), 4097
**	cbhi	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i32_x0_slt_4098:
**	mov	(w[0-9]+), 4097
**	cble	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i32_x0_sle_4098:
**	mov	(w[0-9]+), 4098
**	cble	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i32_x0_sgt_4098:
**	mov	(w[0-9]+), 4098
**	cbgt	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i32_x0_sge_4098:
**	mov	(w[0-9]+), 4097
**	cbgt	w0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u64_x0_eq_4098:
**	mov	x([0-9]+), 4098
**	cbeq	x0, x\1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u64_x0_ne_4098:
**	mov	(x[0-9]+), 4098
**	cbne	x0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u64_x0_ult_4098:
**	mov	(x[0-9]+), 4097
**	cbls	x0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u64_x0_ule_4098:
**	mov	(x[0-9]+), 4098
**	cbls	x0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u64_x0_ugt_4098:
**	mov	(x[0-9]+), 4098
**	cbhi	x0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** u64_x0_uge_4098:
**	mov	(x[0-9]+), 4097
**	cbhi	x0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i64_x0_slt_4098:
**	mov	(x[0-9]+), 4097
**	cble	x0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i64_x0_sle_4098:
**	mov	(x[0-9]+), 4098
**	cble	x0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i64_x0_sgt_4098:
**	mov	(x[0-9]+), 4098
**	cbgt	x0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** i64_x0_sge_4098:
**	mov	(x[0-9]+), 4097
**	cbgt	x0, \1, .L([0-9]+)
**	b	not_taken
** .L\2:
**	b	taken
*/

/*
** far_branch_u8_x0_eq_x1:
**	sub	sp, sp, #16
**	str	wzr, \[sp, 12\]
**	cbbeq	w0|w1, w1|w0, .L([0-9]+)
**	b	.L([0-9]+)
** .L\1:
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
**	str	wzr, \[sp, 12\]
**	cbheq	w0|w1, w1|w0, .L([0-9]+)
**	b	.L([0-9]+)
** .L\1:
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
**	cbeq	w0, w1, .L([0-9]+)
**	b	.L([0-9]+)
** .L\1:
**	str	wzr, \[sp, 12\]
**	...
**	str	wzr, \[sp, 12\]
** .L\2:
**	add	sp, sp, 16
**	b	taken
*/

/*
** far_branch_u64_x0_eq_x1:
**	sub	sp, sp, #16
**	str	wzr, \[sp, 12\]
**	cbeq	x0, x1, .L([0-9]+)
**	b	.L([0-9]+)
** .L\1:
**	str	wzr, \[sp, 12\]
**	...
**	str	wzr, \[sp, 12\]
** .L\2:
**	add	sp, sp, 16
**	b	taken
*/

/*
** far_branch_u8_x0_eq_42:
**	sub	sp, sp, #16
**	mov	w([0-9]+), 42
**	str	wzr, \[sp, 12\]
**	cbbeq	w0, w\1, .L([0-9]+)
**	b	.L([0-9]+)
** .L\2:
**	str	wzr, \[sp, 12\]
**	...
**	str	wzr, \[sp, 12\]
** .L\3:
**	add	sp, sp, 16
**	b	taken
*/

/*
** far_branch_u16_x0_eq_42:
**	sub	sp, sp, #16
**	mov	w([0-9]+), 42
**	str	wzr, \[sp, 12\]
**	cbheq	w0, w\1, .L([0-9]+)
**	b	.L([0-9]+)
** .L\2:
**	str	wzr, \[sp, 12\]
**	...
**	str	wzr, \[sp, 12\]
** .L\3:
**	add	sp, sp, 16
**	b	taken
*/

/*
** far_branch_u32_x0_eq_42:
**	sub	sp, sp, #16
**	str	wzr, \[sp, 12\]
**	cbeq	w0, 42, .L([0-9]+)
**	b	.L([0-9]+)
** .L\1:
**	str	wzr, \[sp, 12\]
**	...
**	str	wzr, \[sp, 12\]
** .L\2:
**	add	sp, sp, 16
**	b	taken
*/

/*
** far_branch_u64_x0_eq_42:
**	sub	sp, sp, #16
**	str	wzr, \[sp, 12\]
**	cbeq	x0, 42, .L([0-9]+)
**	b	.L([0-9]+)
** .L\1:
**	str	wzr, \[sp, 12\]
**	...
**	str	wzr, \[sp, 12\]
** .L\2:
**	add	sp, sp, 16
**	b	taken
*/
