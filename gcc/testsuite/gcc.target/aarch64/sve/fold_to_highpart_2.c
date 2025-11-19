/* { dg-do compile } */
/* { dg-additional-options "-O1" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_neon.h>
#include <arm_sve.h>
#include <arm_neon_sve_bridge.h>

// ============================================================================
// 8 -> 16 : SIGNED
// ============================================================================

/* 
** add_neon_i16_from_i8_low_sve_bridged:
** 	saddl	v0.8h, v0.8b, v1.8b
** 	ret
*/
svint16_t add_neon_i16_from_i8_low_sve_bridged(svint8_t a, svint8_t b) {
    int16x8_t ar = vmovl_s8(vget_low_s8(svget_neonq(a)));
    int16x8_t br = vmovl_s8(vget_low_s8(svget_neonq(b)));
    return svset_neonq_s16(svundef_s16(), vaddq_s16(ar, br));
}

/*
** add_neon_i16_from_i8_high_sve_bridged:
** 	saddl2	v0.8h, v0.16b, v1.16b
** 	ret
*/
svint16_t add_neon_i16_from_i8_high_sve_bridged(svint8_t a, svint8_t b) {
    int16x8_t ar = vmovl_s8(vget_high_s8(svget_neonq(a)));
    int16x8_t br = vmovl_s8(vget_high_s8(svget_neonq(b)));
    return svset_neonq_s16(svundef_s16(), vaddq_s16(ar, br));
}

/*
** sub_neon_i16_from_i8_low_sve_bridged:
** 	ssubl	v0.8h, v0.8b, v1.8b
** 	ret
*/
svint16_t sub_neon_i16_from_i8_low_sve_bridged(svint8_t a, svint8_t b) {
    int16x8_t ar = vmovl_s8(vget_low_s8(svget_neonq(a)));
    int16x8_t br = vmovl_s8(vget_low_s8(svget_neonq(b)));
    return svset_neonq_s16(svundef_s16(), vsubq_s16(ar, br));
}

/*
** sub_neon_i16_from_i8_high_sve_bridged:
** 	ssubl2	v0.8h, v0.16b, v1.16b
** 	ret
*/
svint16_t sub_neon_i16_from_i8_high_sve_bridged(svint8_t a, svint8_t b) {
    int16x8_t ar = vmovl_s8(vget_high_s8(svget_neonq(a)));
    int16x8_t br = vmovl_s8(vget_high_s8(svget_neonq(b)));
    return svset_neonq_s16(svundef_s16(), vsubq_s16(ar, br));
}

// ============================================================================
// 8 -> 16 : UNSIGNED
// ============================================================================

/*
** add_neon_u16_from_u8_low_sve_bridged:
** 	uaddl	v0.8h, v0.8b, v1.8b
** 	ret
*/
svuint16_t add_neon_u16_from_u8_low_sve_bridged(svuint8_t a, svuint8_t b) {
    uint16x8_t ar = vmovl_u8(vget_low_u8(svget_neonq(a)));
    uint16x8_t br = vmovl_u8(vget_low_u8(svget_neonq(b)));
    return svset_neonq_u16(svundef_u16(), vaddq_u16(ar, br));
}

/*
** add_neon_u16_from_u8_high_sve_bridged:
** 	uaddl2	v0.8h, v0.16b, v1.16b
** 	ret
*/
svuint16_t add_neon_u16_from_u8_high_sve_bridged(svuint8_t a, svuint8_t b) {
    uint16x8_t ar = vmovl_u8(vget_high_u8(svget_neonq(a)));
    uint16x8_t br = vmovl_u8(vget_high_u8(svget_neonq(b)));
    return svset_neonq_u16(svundef_u16(), vaddq_u16(ar, br));
}

/*
** sub_neon_u16_from_u8_low_sve_bridged:
** 	usubl	v0.8h, v0.8b, v1.8b
** 	ret
*/
svuint16_t sub_neon_u16_from_u8_low_sve_bridged(svuint8_t a, svuint8_t b) {
    uint16x8_t ar = vmovl_u8(vget_low_u8(svget_neonq(a)));
    uint16x8_t br = vmovl_u8(vget_low_u8(svget_neonq(b)));
    return svset_neonq_u16(svundef_u16(), vsubq_u16(ar, br));
}

/*
** sub_neon_u16_from_u8_high_sve_bridged:
** 	usubl2	v0.8h, v0.16b, v1.16b
** 	ret
*/
svuint16_t sub_neon_u16_from_u8_high_sve_bridged(svuint8_t a, svuint8_t b) {
    uint16x8_t ar = vmovl_u8(vget_high_u8(svget_neonq(a)));
    uint16x8_t br = vmovl_u8(vget_high_u8(svget_neonq(b)));
    return svset_neonq_u16(svundef_u16(), vsubq_u16(ar, br));
}

// ============================================================================
// 16 -> 32 : SIGNED
// ============================================================================

/*
** add_neon_i32_from_i16_low_sve_bridged:
** 	saddl	v0.4s, v0.4h, v1.4h
** 	ret
*/
svint32_t add_neon_i32_from_i16_low_sve_bridged(svint16_t a, svint16_t b) {
    int32x4_t ar = vmovl_s16(vget_low_s16(svget_neonq(a)));
    int32x4_t br = vmovl_s16(vget_low_s16(svget_neonq(b)));
    return svset_neonq_s32(svundef_s32(), vaddq_s32(ar, br));
}

/*
** add_neon_i32_from_i16_high_sve_bridged:
** 	saddl2	v0.4s, v0.8h, v1.8h
** 	ret
*/
svint32_t add_neon_i32_from_i16_high_sve_bridged(svint16_t a, svint16_t b) {
    int32x4_t ar = vmovl_s16(vget_high_s16(svget_neonq(a)));
    int32x4_t br = vmovl_s16(vget_high_s16(svget_neonq(b)));
    return svset_neonq_s32(svundef_s32(), vaddq_s32(ar, br));
}

/*
** sub_neon_i32_from_i16_low_sve_bridged:
** 	ssubl	v0.4s, v0.4h, v1.4h
** 	ret
*/
svint32_t sub_neon_i32_from_i16_low_sve_bridged(svint16_t a, svint16_t b) {
    int32x4_t ar = vmovl_s16(vget_low_s16(svget_neonq(a)));
    int32x4_t br = vmovl_s16(vget_low_s16(svget_neonq(b)));
    return svset_neonq_s32(svundef_s32(), vsubq_s32(ar, br));
}

/*
** sub_neon_i32_from_i16_high_sve_bridged:
** 	ssubl2	v0.4s, v0.8h, v1.8h
** 	ret
*/
svint32_t sub_neon_i32_from_i16_high_sve_bridged(svint16_t a, svint16_t b) {
    int32x4_t ar = vmovl_s16(vget_high_s16(svget_neonq(a)));
    int32x4_t br = vmovl_s16(vget_high_s16(svget_neonq(b)));
    return svset_neonq_s32(svundef_s32(), vsubq_s32(ar, br));
}

// ============================================================================
// 16 -> 32 : UNSIGNED
// ============================================================================

/*
** add_neon_u32_from_u16_low_sve_bridged:
** 	uaddl	v0.4s, v0.4h, v1.4h
** 	ret
*/
svuint32_t add_neon_u32_from_u16_low_sve_bridged(svuint16_t a, svuint16_t b) {
    uint32x4_t ar = vmovl_u16(vget_low_u16(svget_neonq(a)));
    uint32x4_t br = vmovl_u16(vget_low_u16(svget_neonq(b)));
    return svset_neonq_u32(svundef_u32(), vaddq_u32(ar, br));
}

/*
** add_neon_u32_from_u16_high_sve_bridged:
** 	uaddl2	v0.4s, v0.8h, v1.8h
** 	ret
*/
svuint32_t add_neon_u32_from_u16_high_sve_bridged(svuint16_t a, svuint16_t b) {
    uint32x4_t ar = vmovl_u16(vget_high_u16(svget_neonq(a)));
    uint32x4_t br = vmovl_u16(vget_high_u16(svget_neonq(b)));
    return svset_neonq_u32(svundef_u32(), vaddq_u32(ar, br));
}

/*
** sub_neon_u32_from_u16_low_sve_bridged:
** 	usubl	v0.4s, v0.4h, v1.4h
** 	ret
*/
svuint32_t sub_neon_u32_from_u16_low_sve_bridged(svuint16_t a, svuint16_t b) {
    uint32x4_t ar = vmovl_u16(vget_low_u16(svget_neonq(a)));
    uint32x4_t br = vmovl_u16(vget_low_u16(svget_neonq(b)));
    return svset_neonq_u32(svundef_u32(), vsubq_u32(ar, br));
}

/*
** sub_neon_u32_from_u16_high_sve_bridged:
** 	usubl2	v0.4s, v0.8h, v1.8h
** 	ret
*/
svuint32_t sub_neon_u32_from_u16_high_sve_bridged(svuint16_t a, svuint16_t b) {
    uint32x4_t ar = vmovl_u16(vget_high_u16(svget_neonq(a)));
    uint32x4_t br = vmovl_u16(vget_high_u16(svget_neonq(b)));
    return svset_neonq_u32(svundef_u32(), vsubq_u32(ar, br));
}

// ============================================================================
// 32 -> 64 : SIGNED
// ============================================================================

/*
** add_neon_i64_from_i32_low_sve_bridged:
** 	saddl	v0.2d, v0.2s, v1.2s
** 	ret
*/
svint64_t add_neon_i64_from_i32_low_sve_bridged(svint32_t a, svint32_t b) {
    int64x2_t ar = vmovl_s32(vget_low_s32(svget_neonq(a)));
    int64x2_t br = vmovl_s32(vget_low_s32(svget_neonq(b)));
    return svset_neonq_s64(svundef_s64(), vaddq_s64(ar, br));
}

/*
** add_neon_i64_from_i32_high_sve_bridged:
** 	saddl2	v0.2d, v0.4s, v1.4s
** 	ret
*/
svint64_t add_neon_i64_from_i32_high_sve_bridged(svint32_t a, svint32_t b) {
    int64x2_t ar = vmovl_s32(vget_high_s32(svget_neonq(a)));
    int64x2_t br = vmovl_s32(vget_high_s32(svget_neonq(b)));
    return svset_neonq_s64(svundef_s64(), vaddq_s64(ar, br));
}

/*
** sub_neon_i64_from_i32_low_sve_bridged:
** 	ssubl	v0.2d, v0.2s, v1.2s
** 	ret
*/
svint64_t sub_neon_i64_from_i32_low_sve_bridged(svint32_t a, svint32_t b) {
    int64x2_t ar = vmovl_s32(vget_low_s32(svget_neonq(a)));
    int64x2_t br = vmovl_s32(vget_low_s32(svget_neonq(b)));
    return svset_neonq_s64(svundef_s64(), vsubq_s64(ar, br));
}

/*
** sub_neon_i64_from_i32_high_sve_bridged:
** 	ssubl2	v0.2d, v0.4s, v1.4s
** 	ret
*/
svint64_t sub_neon_i64_from_i32_high_sve_bridged(svint32_t a, svint32_t b) {
    int64x2_t ar = vmovl_s32(vget_high_s32(svget_neonq(a)));
    int64x2_t br = vmovl_s32(vget_high_s32(svget_neonq(b)));
    return svset_neonq_s64(svundef_s64(), vsubq_s64(ar, br));
}

// ============================================================================
// 32 -> 64 : UNSIGNED
// ============================================================================

/*
** add_neon_u64_from_u32_low_sve_bridged:
** 	uaddl	v0.2d, v0.2s, v1.2s
** 	ret
*/
svuint64_t add_neon_u64_from_u32_low_sve_bridged(svuint32_t a, svuint32_t b) {
    uint64x2_t ar = vmovl_u32(vget_low_u32(svget_neonq(a)));
    uint64x2_t br = vmovl_u32(vget_low_u32(svget_neonq(b)));
    return svset_neonq_u64(svundef_u64(), vaddq_u64(ar, br));
}

/*
** add_neon_u64_from_u32_high_sve_bridged:
** 	uaddl2	v0.2d, v0.4s, v1.4s
** 	ret
*/
svuint64_t add_neon_u64_from_u32_high_sve_bridged(svuint32_t a, svuint32_t b) {
    uint64x2_t ar = vmovl_u32(vget_high_u32(svget_neonq(a)));
    uint64x2_t br = vmovl_u32(vget_high_u32(svget_neonq(b)));
    return svset_neonq_u64(svundef_u64(), vaddq_u64(ar, br));
}

/*
** sub_neon_u64_from_u32_low_sve_bridged:
** 	usubl	v0.2d, v0.2s, v1.2s
** 	ret
*/
svuint64_t sub_neon_u64_from_u32_low_sve_bridged(svuint32_t a, svuint32_t b) {
    uint64x2_t ar = vmovl_u32(vget_low_u32(svget_neonq(a)));
    uint64x2_t br = vmovl_u32(vget_low_u32(svget_neonq(b)));
    return svset_neonq_u64(svundef_u64(), vsubq_u64(ar, br));
}

/* 
** sub_neon_u64_from_u32_high_sve_bridged:
** 	usubl2	v0.2d, v0.4s, v1.4s
** 	ret
*/
svuint64_t sub_neon_u64_from_u32_high_sve_bridged(svuint32_t a, svuint32_t b) {
    uint64x2_t ar = vmovl_u32(vget_high_u32(svget_neonq(a)));
    uint64x2_t br = vmovl_u32(vget_high_u32(svget_neonq(b)));
    return svset_neonq_u64(svundef_u64(), vsubq_u64(ar, br));
}
