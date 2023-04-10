/* { dg-do compile } */
/* { dg-skip-if "Require optimisation to compile DCE tests" { *-*-* } { "-O0" "-mfloat-abi=softfp" } { "" } } */
/* { dg-require-effective-target arm_v8_1m_main_cde_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_main_cde_mve_fp } */
/* We use -ffast-math so that the addition of 0.0 to a value is assumed to not
   change the value.  This means the tests for float types can use the same
   trick of adding to a value initialised to zero to check whether the RTL
   patterns correctly mark that the incoming value is not used.  */
/* { dg-additional-options "-ffast-math" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "cde-mve-tests.c"

/* NOTE:
     We avoid matching the functions returning a __builtin_neon_ti value since
     there are variations between processors that make matching the whole
     function difficult.
     Since moving a TImode value into an MVE 'Q' register takes a few
     temporaries, this leaves many instructions which can end up being
     scheduled in different ways.  Matching the ways this ends up getting
     scheduled and restructured is awkward, and the extra tests for this one
     data type don't seem to be worth the confusing testcases.  */

/*
** test_cde_vcx1q_u8float16x8_tintint:
** 	vcx1	p0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx1q_u8float32x4_tintint:
** 	vcx1	p0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx1q_u8uint8x16_tintint:
** 	vcx1	p0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx1q_u8uint16x8_tintint:
** 	vcx1	p0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx1q_u8uint32x4_tintint:
** 	vcx1	p0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx1q_u8uint64x2_tintint:
** 	vcx1	p0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx1q_u8int8x16_tintint:
** 	vcx1	p0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx1q_u8int16x8_tintint:
** 	vcx1	p0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx1q_u8int32x4_tintint:
** 	vcx1	p0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx1q_u8int64x2_tintint:
** 	vcx1	p0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx1qafloat16x8_tintint:
** 	vmov\.i32	q0, #0  @ v16qi
** 	vcx1a	p0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx1qafloat32x4_tintint:
** 	vmov\.i32	q0, #0  @ v16qi
** 	vcx1a	p0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx1qauint8x16_tintint:
** 	vmov\.i32	q0, #0  @ v16qi
** 	vcx1a	p0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx1qauint16x8_tintint:
** 	vmov\.i32	q0, #0  @ v16qi
** 	vcx1a	p0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx1qauint32x4_tintint:
** 	vmov\.i32	q0, #0  @ v16qi
** 	vcx1a	p0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx1qauint64x2_tintint:
** 	vmov\.i32	q0, #0  @ v16qi
** 	vcx1a	p0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx1qaint8x16_tintint:
** 	vmov\.i32	q0, #0  @ v16qi
** 	vcx1a	p0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx1qaint16x8_tintint:
** 	vmov\.i32	q0, #0  @ v16qi
** 	vcx1a	p0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx1qaint32x4_tintint:
** 	vmov\.i32	q0, #0  @ v16qi
** 	vcx1a	p0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx1qaint64x2_tintint:
** 	vmov\.i32	q0, #0  @ v16qi
** 	vcx1a	p0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx2q_u8float16x8_tuint16x8_tint:
** 	vcx2	p0, q0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx2q_u8float16x8_tfloat32x4_tint:
** 	vcx2	p0, q0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx2q_u8float32x4_tuint8x16_tint:
** 	vcx2	p0, q0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx2q_u8int64x2_tuint8x16_tint:
** 	vcx2	p0, q0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx2q_u8int8x16_tuint8x16_tint:
** 	vcx2	p0, q0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx2q_u8uint16x8_tuint8x16_tint:
** 	vcx2	p0, q0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx2q_u8uint8x16_tint64x2_tint:
** 	vcx2	p0, q0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx2q_u8uint8x16_tint8x16_tint:
** 	vcx2	p0, q0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx2q_u8uint8x16_tuint16x8_tint:
** 	vcx2	p0, q0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx2q_u8uint8x16_tuint8x16_tint:
** 	vcx2	p0, q0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx2qfloat16x8_tuint16x8_tint:
** 	vcx2	p0, q0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx2qfloat16x8_tfloat32x4_tint:
** 	vcx2	p0, q0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx2qfloat32x4_tuint8x16_tint:
** 	vcx2	p0, q0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx2qint64x2_tuint8x16_tint:
** 	vcx2	p0, q0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx2qint8x16_tuint8x16_tint:
** 	vcx2	p0, q0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx2quint16x8_tuint8x16_tint:
** 	vcx2	p0, q0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx2quint8x16_tint64x2_tint:
** 	vcx2	p0, q0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx2quint8x16_tint8x16_tint:
** 	vcx2	p0, q0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx2quint8x16_tuint16x8_tint:
** 	vcx2	p0, q0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx2quint8x16_tuint8x16_tint:
** 	vcx2	p0, q0, q0, #33
** 	bx	lr
*/
/*
** test_cde_vcx2qafloat16x8_tuint16x8_tint:
** 	vmov\.i32	(q[1-7]), #0  @ v16qi
** 	vcx2a	p0, \1, q0, #33
** 	vmov	q0, \1
** 	bx	lr
*/
/*
** test_cde_vcx2qafloat16x8_tfloat32x4_tint:
** 	vmov\.i32	(q[1-7]), #0  @ v16qi
** 	vcx2a	p0, \1, q0, #33
** 	vmov	q0, \1
** 	bx	lr
*/
/*
** test_cde_vcx2qafloat32x4_tuint8x16_tint:
** 	vmov\.i32	(q[1-7]), #0  @ v16qi
** 	vcx2a	p0, \1, q0, #33
** 	vmov	q0, \1
** 	bx	lr
*/
/*
** test_cde_vcx2qaint64x2_tuint8x16_tint:
** 	vmov\.i32	(q[1-7]), #0  @ v16qi
** 	vcx2a	p0, \1, q0, #33
** 	vmov	q0, \1
** 	bx	lr
*/
/*
** test_cde_vcx2qaint8x16_tuint8x16_tint:
** 	vmov\.i32	(q[1-7]), #0  @ v16qi
** 	vcx2a	p0, \1, q0, #33
** 	vmov	q0, \1
** 	bx	lr
*/
/*
** test_cde_vcx2qauint16x8_tuint8x16_tint:
** 	vmov\.i32	(q[1-7]), #0  @ v16qi
** 	vcx2a	p0, \1, q0, #33
** 	vmov	q0, \1
** 	bx	lr
*/
/*
** test_cde_vcx2qauint8x16_tint64x2_tint:
** 	vmov\.i32	(q[1-7]), #0  @ v16qi
** 	vcx2a	p0, \1, q0, #33
** 	vmov	q0, \1
** 	bx	lr
*/
/*
** test_cde_vcx2qauint8x16_tint8x16_tint:
** 	vmov\.i32	(q[1-7]), #0  @ v16qi
** 	vcx2a	p0, \1, q0, #33
** 	vmov	q0, \1
** 	bx	lr
*/
/*
** test_cde_vcx2qauint8x16_tuint16x8_tint:
** 	vmov\.i32	(q[1-7]), #0  @ v16qi
** 	vcx2a	p0, \1, q0, #33
** 	vmov	q0, \1
** 	bx	lr
*/
/*
** test_cde_vcx2qauint8x16_tuint8x16_tint:
** 	vmov\.i32	(q[1-7]), #0  @ v16qi
** 	vcx2a	p0, \1, q0, #33
** 	vmov	q0, \1
** 	bx	lr
*/
/*
** test_cde_vcx3q_u8uint8x16_tuint8x16_tuint8x16_t:
** 	vcx3	p0, q0, q0, q1, #12
** 	bx	lr
*/
/*
** test_cde_vcx3q_u8uint16x8_tuint8x16_tuint8x16_t:
** 	vcx3	p0, q0, q0, q1, #12
** 	bx	lr
*/
/*
** test_cde_vcx3q_u8uint8x16_tuint16x8_tuint8x16_t:
** 	vcx3	p0, q0, q0, q1, #12
** 	bx	lr
*/
/*
** test_cde_vcx3q_u8uint8x16_tuint8x16_tuint16x8_t:
** 	vcx3	p0, q0, q0, q1, #12
** 	bx	lr
*/
/*
** test_cde_vcx3q_u8float16x8_tfloat16x8_tfloat16x8_t:
** 	vcx3	p0, q0, q0, q1, #12
** 	bx	lr
*/
/*
** test_cde_vcx3q_u8float32x4_tuint64x2_tfloat16x8_t:
** 	vcx3	p0, q0, q0, q1, #12
** 	bx	lr
*/
/*
** test_cde_vcx3q_u8int8x16_tuint8x16_tuint8x16_t:
** 	vcx3	p0, q0, q0, q1, #12
** 	bx	lr
*/
/*
** test_cde_vcx3q_u8uint8x16_tint8x16_tuint8x16_t:
** 	vcx3	p0, q0, q0, q1, #12
** 	bx	lr
*/
/*
** test_cde_vcx3q_u8uint8x16_tuint8x16_tint8x16_t:
** 	vcx3	p0, q0, q0, q1, #12
** 	bx	lr
*/
/*
** test_cde_vcx3q_u8int64x2_tuint8x16_tuint8x16_t:
** 	vcx3	p0, q0, q0, q1, #12
** 	bx	lr
*/
/*
** test_cde_vcx3q_u8uint8x16_tint64x2_tuint8x16_t:
** 	vcx3	p0, q0, q0, q1, #12
** 	bx	lr
*/
/*
** test_cde_vcx3q_u8uint8x16_tuint8x16_tint64x2_t:
** 	vcx3	p0, q0, q0, q1, #12
** 	bx	lr
*/
/*
** test_cde_vcx3q_u8uint8x16_tint64x2_tint64x2_t:
** 	vcx3	p0, q0, q0, q1, #12
** 	bx	lr
*/
/*
** test_cde_vcx3quint8x16_tuint8x16_tuint8x16_t:
** 	vcx3	p0, q0, q0, q1, #12
** 	bx	lr
*/
/*
** test_cde_vcx3qfloat16x8_tfloat16x8_tfloat16x8_t:
** 	vcx3	p0, q0, q0, q1, #12
** 	bx	lr
*/
/*
** test_cde_vcx3qfloat32x4_tuint64x2_tfloat16x8_t:
** 	vcx3	p0, q0, q0, q1, #12
** 	bx	lr
*/
/*
** test_cde_vcx3quint16x8_tuint8x16_tuint8x16_t:
** 	vcx3	p0, q0, q0, q1, #12
** 	bx	lr
*/
/*
** test_cde_vcx3quint8x16_tuint16x8_tuint8x16_t:
** 	vcx3	p0, q0, q0, q1, #12
** 	bx	lr
*/
/*
** test_cde_vcx3quint8x16_tuint8x16_tuint16x8_t:
** 	vcx3	p0, q0, q0, q1, #12
** 	bx	lr
*/
/*
** test_cde_vcx3qint8x16_tuint8x16_tuint8x16_t:
** 	vcx3	p0, q0, q0, q1, #12
** 	bx	lr
*/
/*
** test_cde_vcx3quint8x16_tint8x16_tuint8x16_t:
** 	vcx3	p0, q0, q0, q1, #12
** 	bx	lr
*/
/*
** test_cde_vcx3quint8x16_tuint8x16_tint8x16_t:
** 	vcx3	p0, q0, q0, q1, #12
** 	bx	lr
*/
/*
** test_cde_vcx3qint64x2_tuint8x16_tuint8x16_t:
** 	vcx3	p0, q0, q0, q1, #12
** 	bx	lr
*/
/*
** test_cde_vcx3quint8x16_tint64x2_tuint8x16_t:
** 	vcx3	p0, q0, q0, q1, #12
** 	bx	lr
*/
/*
** test_cde_vcx3quint8x16_tuint8x16_tint64x2_t:
** 	vcx3	p0, q0, q0, q1, #12
** 	bx	lr
*/
/*
** test_cde_vcx3quint8x16_tint64x2_tint64x2_t:
** 	vcx3	p0, q0, q0, q1, #12
** 	bx	lr
*/
/*
** test_cde_vcx3qauint8x16_tuint8x16_tuint8x16_t:
** 	vmov\.i32	(q[2-7]), #0  @ v16qi
** 	vcx3a	p0, \1, q0, q1, #12
** 	vmov	q0, \1
** 	bx	lr
*/
/*
** test_cde_vcx3qafloat16x8_tfloat16x8_tfloat16x8_t:
** 	vmov\.i32	(q[2-7]), #0  @ v16qi
** 	vcx3a	p0, \1, q0, q1, #12
** 	vmov	q0, \1
** 	bx	lr
*/
/*
** test_cde_vcx3qafloat32x4_tuint64x2_tfloat16x8_t:
** 	vmov\.i32	(q[2-7]), #0  @ v16qi
** 	vcx3a	p0, \1, q0, q1, #12
** 	vmov	q0, \1
** 	bx	lr
*/
/*
** test_cde_vcx3qauint16x8_tuint8x16_tuint8x16_t:
** 	vmov\.i32	(q[2-7]), #0  @ v16qi
** 	vcx3a	p0, \1, q0, q1, #12
** 	vmov	q0, \1
** 	bx	lr
*/
/*
** test_cde_vcx3qauint8x16_tuint16x8_tuint8x16_t:
** 	vmov\.i32	(q[2-7]), #0  @ v16qi
** 	vcx3a	p0, \1, q0, q1, #12
** 	vmov	q0, \1
** 	bx	lr
*/
/*
** test_cde_vcx3qauint8x16_tuint8x16_tuint16x8_t:
** 	vmov\.i32	(q[2-7]), #0  @ v16qi
** 	vcx3a	p0, \1, q0, q1, #12
** 	vmov	q0, \1
** 	bx	lr
*/
/*
** test_cde_vcx3qaint8x16_tuint8x16_tuint8x16_t:
** 	vmov\.i32	(q[2-7]), #0  @ v16qi
** 	vcx3a	p0, \1, q0, q1, #12
** 	vmov	q0, \1
** 	bx	lr
*/
/*
** test_cde_vcx3qauint8x16_tint8x16_tuint8x16_t:
** 	vmov\.i32	(q[2-7]), #0  @ v16qi
** 	vcx3a	p0, \1, q0, q1, #12
** 	vmov	q0, \1
** 	bx	lr
*/
/*
** test_cde_vcx3qauint8x16_tuint8x16_tint8x16_t:
** 	vmov\.i32	(q[2-7]), #0  @ v16qi
** 	vcx3a	p0, \1, q0, q1, #12
** 	vmov	q0, \1
** 	bx	lr
*/
/*
** test_cde_vcx3qaint64x2_tuint8x16_tuint8x16_t:
** 	vmov\.i32	(q[2-7]), #0  @ v16qi
** 	vcx3a	p0, \1, q0, q1, #12
** 	vmov	q0, \1
** 	bx	lr
*/
/*
** test_cde_vcx3qauint8x16_tint64x2_tuint8x16_t:
** 	vmov\.i32	(q[2-7]), #0  @ v16qi
** 	vcx3a	p0, \1, q0, q1, #12
** 	vmov	q0, \1
** 	bx	lr
*/
/*
** test_cde_vcx3qauint8x16_tuint8x16_tint64x2_t:
** 	vmov\.i32	(q[2-7]), #0  @ v16qi
** 	vcx3a	p0, \1, q0, q1, #12
** 	vmov	q0, \1
** 	bx	lr
*/
/*
** test_cde_vcx3qauint8x16_tint64x2_tint64x2_t:
** 	vmov\.i32	(q[2-7]), #0  @ v16qi
** 	vcx3a	p0, \1, q0, q1, #12
** 	vmov	q0, \1
** 	bx	lr
*/

/* Predicated MVE intrinsics.  */
/* Merging lane predication types.
   NOTE: Depending on the target, the setup instructions (vmov's and vmsr) can
   be in a different order.  Here we just check that all the expected setup
   instructions are there.  We don't check that the setup instructions are
   different since the likelyhood of the compiler generating repeated versions
   of one rather than one and the other is very low and it's difficult to apply
   such a constraint in TCL regexps (lookahead/lookbehind constraints may not
   contain back references).  */
/*
** test_cde_vcx1q_mfloat16x8_tintint:
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	vpst
** 	vcx1t	p0, q0, #32
** 	bx	lr
*/
/*
** test_cde_vcx1q_mfloat32x4_tintint:
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	vpst
** 	vcx1t	p0, q0, #32
** 	bx	lr
*/
/*
** test_cde_vcx1q_muint8x16_tintint:
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	vpst
** 	vcx1t	p0, q0, #32
** 	bx	lr
*/
/*
** test_cde_vcx1q_muint16x8_tintint:
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	vpst
** 	vcx1t	p0, q0, #32
** 	bx	lr
*/
/*
** test_cde_vcx1q_muint32x4_tintint:
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	vpst
** 	vcx1t	p0, q0, #32
** 	bx	lr
*/
/*
** test_cde_vcx1q_muint64x2_tintint:
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	vpst
** 	vcx1t	p0, q0, #32
** 	bx	lr
*/
/*
** test_cde_vcx1q_mint8x16_tintint:
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	vpst
** 	vcx1t	p0, q0, #32
** 	bx	lr
*/
/*
** test_cde_vcx1q_mint16x8_tintint:
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	vpst
** 	vcx1t	p0, q0, #32
** 	bx	lr
*/
/*
** test_cde_vcx1q_mint32x4_tintint:
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	vpst
** 	vcx1t	p0, q0, #32
** 	bx	lr
*/
/*
** test_cde_vcx1q_mint64x2_tintint:
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	vpst
** 	vcx1t	p0, q0, #32
** 	bx	lr
*/


/*
** test_cde_vcx1qa_mfloat16x8_tintint:
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	vpst
** 	vcx1at	p0, q0, #32
** 	bx	lr
*/
/*
** test_cde_vcx1qa_mfloat32x4_tintint:
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	vpst
** 	vcx1at	p0, q0, #32
** 	bx	lr
*/
/*
** test_cde_vcx1qa_muint8x16_tintint:
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	vpst
** 	vcx1at	p0, q0, #32
** 	bx	lr
*/
/*
** test_cde_vcx1qa_muint16x8_tintint:
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	vpst
** 	vcx1at	p0, q0, #32
** 	bx	lr
*/
/*
** test_cde_vcx1qa_muint32x4_tintint:
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	vpst
** 	vcx1at	p0, q0, #32
** 	bx	lr
*/
/*
** test_cde_vcx1qa_muint64x2_tintint:
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	vpst
** 	vcx1at	p0, q0, #32
** 	bx	lr
*/
/*
** test_cde_vcx1qa_mint8x16_tintint:
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	vpst
** 	vcx1at	p0, q0, #32
** 	bx	lr
*/
/*
** test_cde_vcx1qa_mint16x8_tintint:
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	vpst
** 	vcx1at	p0, q0, #32
** 	bx	lr
*/
/*
** test_cde_vcx1qa_mint32x4_tintint:
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	vpst
** 	vcx1at	p0, q0, #32
** 	bx	lr
*/
/*
** test_cde_vcx1qa_mint64x2_tintint:
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	(?:vmov\.i32	q0, #0  @ v16qi|vmsr	p0, r2	@ movhi)
** 	vpst
** 	vcx1at	p0, q0, #32
** 	bx	lr
*/


/*
** test_cde_vcx2q_mfloat16x8_tuint16x8_tint:
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	vpst
** 	vcx2t	p0, (q[1-7]), q0, #32
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx2q_mfloat16x8_tfloat32x4_tint:
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	vpst
** 	vcx2t	p0, (q[1-7]), q0, #32
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx2q_mfloat32x4_tuint8x16_tint:
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	vpst
** 	vcx2t	p0, (q[1-7]), q0, #32
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx2q_mint64x2_tuint8x16_tint:
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	vpst
** 	vcx2t	p0, (q[1-7]), q0, #32
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx2q_mint8x16_tuint8x16_tint:
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	vpst
** 	vcx2t	p0, (q[1-7]), q0, #32
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx2q_muint16x8_tuint8x16_tint:
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	vpst
** 	vcx2t	p0, (q[1-7]), q0, #32
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx2q_muint8x16_tint64x2_tint:
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	vpst
** 	vcx2t	p0, (q[1-7]), q0, #32
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx2q_muint8x16_tint8x16_tint:
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	vpst
** 	vcx2t	p0, (q[1-7]), q0, #32
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx2q_muint8x16_tuint16x8_tint:
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	vpst
** 	vcx2t	p0, (q[1-7]), q0, #32
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx2q_muint8x16_tuint8x16_tint:
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	vpst
** 	vcx2t	p0, (q[1-7]), q0, #32
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/


/*
** test_cde_vcx2qa_mfloat16x8_tuint16x8_tint:
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	vpst
** 	vcx2at	p0, (q[1-7]), q0, #32
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx2qa_mfloat16x8_tfloat32x4_tint:
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	vpst
** 	vcx2at	p0, (q[1-7]), q0, #32
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx2qa_mfloat32x4_tuint8x16_tint:
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	vpst
** 	vcx2at	p0, (q[1-7]), q0, #32
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx2qa_mint64x2_tuint8x16_tint:
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	vpst
** 	vcx2at	p0, (q[1-7]), q0, #32
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx2qa_mint8x16_tuint8x16_tint:
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	vpst
** 	vcx2at	p0, (q[1-7]), q0, #32
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx2qa_muint16x8_tuint8x16_tint:
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	vpst
** 	vcx2at	p0, (q[1-7]), q0, #32
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx2qa_muint8x16_tint64x2_tint:
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	vpst
** 	vcx2at	p0, (q[1-7]), q0, #32
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx2qa_muint8x16_tint8x16_tint:
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	vpst
** 	vcx2at	p0, (q[1-7]), q0, #32
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx2qa_muint8x16_tuint16x8_tint:
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	vpst
** 	vcx2at	p0, (q[1-7]), q0, #32
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx2qa_muint8x16_tuint8x16_tint:
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	(?:vmov\.i32	q[1-7], #0  @ v16qi|vmsr	p0, r1	@ movhi)
** 	vpst
** 	vcx2at	p0, (q[1-7]), q0, #32
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/


/*
** test_cde_vcx3q_muint8x16_tuint8x16_tuint8x16_t:
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	vpst
** 	vcx3t	p0, (q[2-7]), q0, q1, #15
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx3q_mfloat16x8_tfloat16x8_tfloat16x8_t:
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	vpst
** 	vcx3t	p0, (q[2-7]), q0, q1, #15
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx3q_mfloat32x4_tuint64x2_tfloat16x8_t:
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	vpst
** 	vcx3t	p0, (q[2-7]), q0, q1, #15
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx3q_muint16x8_tuint8x16_tuint8x16_t:
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	vpst
** 	vcx3t	p0, (q[2-7]), q0, q1, #15
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx3q_muint8x16_tuint16x8_tuint8x16_t:
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	vpst
** 	vcx3t	p0, (q[2-7]), q0, q1, #15
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx3q_muint8x16_tuint8x16_tuint16x8_t:
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	vpst
** 	vcx3t	p0, (q[2-7]), q0, q1, #15
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx3q_mint8x16_tuint8x16_tuint8x16_t:
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	vpst
** 	vcx3t	p0, (q[2-7]), q0, q1, #15
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx3q_muint8x16_tint8x16_tuint8x16_t:
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	vpst
** 	vcx3t	p0, (q[2-7]), q0, q1, #15
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx3q_muint8x16_tuint8x16_tint8x16_t:
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	vpst
** 	vcx3t	p0, (q[2-7]), q0, q1, #15
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx3q_mint64x2_tuint8x16_tuint8x16_t:
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	vpst
** 	vcx3t	p0, (q[2-7]), q0, q1, #15
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx3q_muint8x16_tint64x2_tuint8x16_t:
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	vpst
** 	vcx3t	p0, (q[2-7]), q0, q1, #15
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx3q_muint8x16_tuint8x16_tint64x2_t:
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	vpst
** 	vcx3t	p0, (q[2-7]), q0, q1, #15
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx3q_muint8x16_tint64x2_tint64x2_t:
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	vpst
** 	vcx3t	p0, (q[2-7]), q0, q1, #15
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/


/*
** test_cde_vcx3qa_muint8x16_tuint8x16_tuint8x16_t:
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	vpst
** 	vcx3at	p0, (q[2-7]), q0, q1, #15
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx3qa_mfloat16x8_tfloat16x8_tfloat16x8_t:
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	vpst
** 	vcx3at	p0, (q[2-7]), q0, q1, #15
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx3qa_mfloat32x4_tuint64x2_tfloat16x8_t:
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	vpst
** 	vcx3at	p0, (q[2-7]), q0, q1, #15
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx3qa_muint16x8_tuint8x16_tuint8x16_t:
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	vpst
** 	vcx3at	p0, (q[2-7]), q0, q1, #15
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx3qa_muint8x16_tuint16x8_tuint8x16_t:
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	vpst
** 	vcx3at	p0, (q[2-7]), q0, q1, #15
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx3qa_muint8x16_tuint8x16_tuint16x8_t:
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	vpst
** 	vcx3at	p0, (q[2-7]), q0, q1, #15
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx3qa_mint8x16_tuint8x16_tuint8x16_t:
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	vpst
** 	vcx3at	p0, (q[2-7]), q0, q1, #15
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx3qa_muint8x16_tint8x16_tuint8x16_t:
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	vpst
** 	vcx3at	p0, (q[2-7]), q0, q1, #15
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx3qa_muint8x16_tuint8x16_tint8x16_t:
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	vpst
** 	vcx3at	p0, (q[2-7]), q0, q1, #15
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx3qa_mint64x2_tuint8x16_tuint8x16_t:
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	vpst
** 	vcx3at	p0, (q[2-7]), q0, q1, #15
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx3qa_muint8x16_tint64x2_tuint8x16_t:
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	vpst
** 	vcx3at	p0, (q[2-7]), q0, q1, #15
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx3qa_muint8x16_tuint8x16_tint64x2_t:
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	vpst
** 	vcx3at	p0, (q[2-7]), q0, q1, #15
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
/*
** test_cde_vcx3qa_muint8x16_tint64x2_tint64x2_t:
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	(?:vmov\.i32	q[2-7], #0  @ v16qi|vmsr	p0, r0	@ movhi)
** 	vpst
** 	vcx3at	p0, (q[2-7]), q0, q1, #15
** 	vmov	q0, \1([[:space:]]+@ [^\n]*)?
** 	bx	lr
*/
