/* HP-UX libunwind.so doesn't provide _UA_END_OF_STACK */
/* { dg-do run } */
/* { dg-options "-fexceptions" } */
/* { dg-skip-if "" { "ia64-*-hpux11.*" }  { "*" } { "" } } */
/* Verify DW_OP_* handling in the unwinder.  */

#include <unwind.h>
#include <stdlib.h>
#include <string.h>

/* #define OP_addr(x) 0x06, ... */
#define OP_deref 0x06,
#define SLEB128(x) (x)&0x7f	/* Assume here the value is -0x40 ... 0x3f.  */
#define ULEB128(x) (x)&0x7f	/* Assume here the value is 0 ... 0x7f.  */
#define VAL1(x) (x)&0xff
#if defined (__BIG_ENDIAN__)
#define VAL2(x) ((x)>>8)&0xff,(x)&0xff
#define VAL4(x) ((x)>>24)&0xff,((x)>>16)&0xff,((x)>>8)&0xff,(x)&0xff
#define VAL8(x) ((x)>>56)&0xff,((x)>>48)&0xff,((x)>>40)&0xff,((x)>>32)&0xff,((x)>>24)&0xff,((x)>>16)&0xff,((x)>>8)&0xff,(x)&0xff
#elif defined(__LITTLE_ENDIAN__) || defined(__x86_64__) || defined(__i386__)
#define VAL2(x) (x)&0xff,((x)>>8)&0xff
#define VAL4(x) (x)&0xff,((x)>>8)&0xff,((x)>>16)&0xff,((x)>>24)&0xff
#define VAL8(x) (x)&0xff,((x)>>8)&0xff,((x)>>16)&0xff,((x)>>24)&0xff,((x)>>32)&0xff,((x)>>40)&0xff,((x)>>48)&0xff,((x)>>56)&0xff
#endif
#define OP_const1u(x) 0x08,VAL1(x),
#define OP_const1s(x) 0x09,VAL1(x),
#define OP_const2u(x) 0x0a,VAL2(x),
#define OP_const2s(x) 0x0b,VAL2(x),
#define OP_const4u(x) 0x0c,VAL4(x),
#define OP_const4s(x) 0x0d,VAL4(x),
#define OP_const8u(x) 0x0e,VAL8(x),
#define OP_const8s(x) 0x0f,VAL8(x),
#define OP_constu(x) 0x10,ULEB128(x),
#define OP_consts(x) 0x11,SLEB128(x),
#define OP_dup 0x12,
#define OP_drop 0x13,
#define OP_over 0x14,
#define OP_pick(x) 0x15,VAL1(x),
#define OP_swap 0x16,
#define OP_rot 0x17,
#define OP_xderef 0x18,
#define OP_abs 0x19,
#define OP_and 0x1a,
#define OP_div 0x1b,
#define OP_minus 0x1c,
#define OP_mod 0x1d,
#define OP_mul 0x1e,
#define OP_neg 0x1f,
#define OP_not 0x20,
#define OP_or 0x21,
#define OP_plus 0x22,
#define OP_plus_uconst(x) 0x23,ULEB128(x),
#define OP_shl 0x24,
#define OP_shr 0x25,
#define OP_shra 0x26,
#define OP_xor 0x27,
#define OP_bra(x) 0x28,VAL2(x),
#define OP_eq 0x29,
#define OP_ge 0x2a,
#define OP_gt 0x2b,
#define OP_le 0x2c,
#define OP_lt 0x2d,
#define OP_ne 0x2e,
#define OP_skip(x) 0x2f,VAL2(x),
#define OP_lit0 0x30,
#define OP_lit1 0x31,
#define OP_lit2 0x32,
#define OP_lit3 0x33,
#define OP_lit4 0x34,
#define OP_lit5 0x35,
#define OP_lit6 0x36,
#define OP_lit7 0x37,
#define OP_lit8 0x38,
#define OP_lit9 0x39,
#define OP_lit10 0x3a,
#define OP_lit11 0x3b,
#define OP_lit12 0x3c,
#define OP_lit13 0x3d,
#define OP_lit14 0x3e,
#define OP_lit15 0x3f,
#define OP_lit16 0x40,
#define OP_lit17 0x41,
#define OP_lit18 0x42,
#define OP_lit19 0x43,
#define OP_lit20 0x44,
#define OP_lit21 0x45,
#define OP_lit22 0x46,
#define OP_lit23 0x47,
#define OP_lit24 0x48,
#define OP_lit25 0x49,
#define OP_lit26 0x4a,
#define OP_lit27 0x4b,
#define OP_lit28 0x4c,
#define OP_lit29 0x4d,
#define OP_lit30 0x4e,
#define OP_lit31 0x4f,
#define OP_reg0 0x50,
#define OP_reg1 0x51,
#define OP_reg2 0x52,
#define OP_reg3 0x53,
#define OP_reg4 0x54,
#define OP_reg5 0x55,
#define OP_reg6 0x56,
#define OP_reg7 0x57,
#define OP_reg8 0x58,
#define OP_reg9 0x59,
#define OP_reg10 0x5a,
#define OP_reg11 0x5b,
#define OP_reg12 0x5c,
#define OP_reg13 0x5d,
#define OP_reg14 0x5e,
#define OP_reg15 0x5f,
#define OP_reg16 0x60,
#define OP_reg17 0x61,
#define OP_reg18 0x62,
#define OP_reg19 0x63,
#define OP_reg20 0x64,
#define OP_reg21 0x65,
#define OP_reg22 0x66,
#define OP_reg23 0x67,
#define OP_reg24 0x68,
#define OP_reg25 0x69,
#define OP_reg26 0x6a,
#define OP_reg27 0x6b,
#define OP_reg28 0x6c,
#define OP_reg29 0x6d,
#define OP_reg30 0x6e,
#define OP_reg31 0x6f,
#define OP_breg0(x) 0x70,SLEB128(x),
#define OP_breg1(x) 0x71,SLEB128(x),
#define OP_breg2(x) 0x72,SLEB128(x),
#define OP_breg3(x) 0x73,SLEB128(x),
#define OP_breg4(x) 0x74,SLEB128(x),
#define OP_breg5(x) 0x75,SLEB128(x),
#define OP_breg6(x) 0x76,SLEB128(x),
#define OP_breg7(x) 0x77,SLEB128(x),
#define OP_breg8(x) 0x78,SLEB128(x),
#define OP_breg9(x) 0x79,SLEB128(x),
#define OP_breg10(x) 0x7a,SLEB128(x),
#define OP_breg11(x) 0x7b,SLEB128(x),
#define OP_breg12(x) 0x7c,SLEB128(x),
#define OP_breg13(x) 0x7d,SLEB128(x),
#define OP_breg14(x) 0x7e,SLEB128(x),
#define OP_breg15(x) 0x7f,SLEB128(x),
#define OP_breg16(x) 0x80,SLEB128(x),
#define OP_breg17(x) 0x81,SLEB128(x),
#define OP_breg18(x) 0x82,SLEB128(x),
#define OP_breg19(x) 0x83,SLEB128(x),
#define OP_breg20(x) 0x84,SLEB128(x),
#define OP_breg21(x) 0x85,SLEB128(x),
#define OP_breg22(x) 0x86,SLEB128(x),
#define OP_breg23(x) 0x87,SLEB128(x),
#define OP_breg24(x) 0x88,SLEB128(x),
#define OP_breg25(x) 0x89,SLEB128(x),
#define OP_breg26(x) 0x8a,SLEB128(x),
#define OP_breg27(x) 0x8b,SLEB128(x),
#define OP_breg28(x) 0x8c,SLEB128(x),
#define OP_breg29(x) 0x8d,SLEB128(x),
#define OP_breg30(x) 0x8e,SLEB128(x),
#define OP_breg31(x) 0x8f,SLEB128(x),
#define OP_regx(x) 0x90,SLEB128(x),
#define OP_fbreg(x) 0x91,SLEB128(x),
#define OP_bregx(x,y) 0x92,ULEB128(x),SLEB128(y),
#define OP_piece(x) 0x93,ULEB128(x),
#define OP_deref_size(x) 0x94,VAL1(x),
#define OP_xderef_size(x) 0x95,VAL1(x),
#define OP_nop 0x96,
#define OP_nop_termination 0x96
#define OP_push_object_address 0x97,
#define OP_call2(x) 0x98,VAL2(x),
#define OP_call4(x) 0x99,VAL4(x),
/* #define OP_call_ref(x) 0x9a,... */
#define OP_form_tls_address(x) 0x9b,
#define OP_call_frame_cfa 0x9c,
#define OP_bit_piece(x) 0x9d,ULEB128(x),
/* #define OP_implicit_value(x...) 0x9e,... */
#define OP_stack_value 0x9f,
#define OP_GNU_push_tls_address 0xe0,
/* #define OP_GNU_encoded_addr(x...) 0xf1, */

#define ASSERT_TOS_NON0 OP_bra(3) OP_skip(-3)
#define ASSERT_TOS_0 OP_lit0 OP_eq ASSERT_TOS_NON0

/* Initially there is CFA value on the stack, we want to
   keep it there at the end.  */
#define CFI_PROGRAM \
OP_lit0 OP_nop ASSERT_TOS_0						\
OP_lit1 ASSERT_TOS_NON0							\
OP_lit1 OP_const1u(1) OP_eq ASSERT_TOS_NON0				\
OP_lit16 OP_const2u(16) OP_eq ASSERT_TOS_NON0				\
OP_lit31 OP_const4u(31) OP_ne ASSERT_TOS_0				\
OP_lit1 OP_neg OP_const1s(-1) OP_eq ASSERT_TOS_NON0			\
OP_lit16 OP_neg OP_const2s(-16) OP_ne ASSERT_TOS_0			\
OP_lit31 OP_const4s(-31) OP_neg OP_ne ASSERT_TOS_0			\
OP_lit7 OP_dup OP_plus_uconst(2) OP_lit9 OP_eq ASSERT_TOS_NON0		\
  OP_lit7 OP_eq ASSERT_TOS_NON0						\
OP_lit20 OP_lit1 OP_drop OP_lit20 OP_eq ASSERT_TOS_NON0			\
OP_lit17 OP_lit19 OP_over OP_lit17 OP_eq ASSERT_TOS_NON0		\
  OP_lit19 OP_eq ASSERT_TOS_NON0 OP_lit17 OP_eq ASSERT_TOS_NON0		\
OP_lit1 OP_lit2 OP_lit3 OP_lit4 OP_pick(2) OP_lit2 OP_eq ASSERT_TOS_NON0\
  OP_lit4 OP_eq ASSERT_TOS_NON0 OP_lit3 OP_eq ASSERT_TOS_NON0		\
  OP_pick(0) OP_lit2 OP_eq ASSERT_TOS_NON0				\
  OP_lit2 OP_eq ASSERT_TOS_NON0 OP_lit1 OP_eq ASSERT_TOS_NON0		\
OP_lit6 OP_lit12 OP_swap OP_lit6 OP_eq ASSERT_TOS_NON0			\
  OP_lit12 OP_eq ASSERT_TOS_NON0					\
OP_lit7 OP_lit8 OP_lit9 OP_rot OP_lit8 OP_eq ASSERT_TOS_NON0		\
  OP_lit7 OP_eq ASSERT_TOS_NON0 OP_lit9 OP_eq ASSERT_TOS_NON0		\
OP_lit7 OP_abs OP_lit7 OP_eq ASSERT_TOS_NON0				\
OP_const1s(-123) OP_abs OP_const1u(123) OP_eq ASSERT_TOS_NON0		\
OP_lit3 OP_lit6 OP_and OP_lit2 OP_eq ASSERT_TOS_NON0			\
OP_lit3 OP_lit6 OP_or OP_lit7 OP_eq ASSERT_TOS_NON0			\
OP_lit17 OP_lit2 OP_minus OP_lit15 OP_eq ASSERT_TOS_NON0		\
/* Divide is signed truncating toward zero.  */				\
OP_const1s(-6) OP_const1s(-2) OP_div OP_lit3 OP_eq ASSERT_TOS_NON0	\
OP_const1s(-7) OP_const1s(3) OP_div OP_const1s(-2)			\
  OP_eq ASSERT_TOS_NON0							\
/* Modulo is unsigned.  */						\
OP_const1s(-6) OP_const1s(-4) OP_mod OP_const1s(-6)			\
  OP_eq ASSERT_TOS_NON0							\
OP_const1s(-6) OP_lit4 OP_mod OP_lit2 OP_eq ASSERT_TOS_NON0		\
OP_lit6 OP_const1s(-4) OP_mod OP_lit6 OP_eq ASSERT_TOS_NON0		\
/* Signed modulo can be implemented using "over over div mul minus".  */\
OP_const1s(-6) OP_const1s(-4) OP_over OP_over OP_div OP_mul OP_minus	\
  OP_const1s(-2) OP_eq ASSERT_TOS_NON0					\
OP_const1s(-7) OP_lit3 OP_over OP_over OP_div OP_mul OP_minus		\
  OP_const1s(-1) OP_eq ASSERT_TOS_NON0					\
OP_lit7 OP_const1s(-3) OP_over OP_over OP_div OP_mul OP_minus		\
  OP_lit1 OP_eq ASSERT_TOS_NON0						\
OP_lit16 OP_lit31 OP_plus_uconst(1) OP_mul OP_const2u(512)		\
  OP_eq ASSERT_TOS_NON0							\
OP_lit5 OP_not OP_lit31 OP_and OP_lit26 OP_eq ASSERT_TOS_NON0		\
OP_lit12 OP_lit31 OP_plus OP_const1u(43) OP_eq ASSERT_TOS_NON0		\
OP_const1s(-6) OP_lit2 OP_plus OP_const1s(-4) OP_eq ASSERT_TOS_NON0	\
OP_const1s(-6) OP_plus_uconst(3) OP_const1s(-3) OP_eq ASSERT_TOS_NON0	\
OP_lit16 OP_lit4 OP_shl OP_const2u(256) OP_eq ASSERT_TOS_NON0		\
OP_lit16 OP_lit3 OP_shr OP_lit2 OP_eq ASSERT_TOS_NON0			\
OP_const1s(-16) OP_lit3 OP_shra OP_const1s(-2) OP_eq ASSERT_TOS_NON0	\
OP_lit3 OP_lit6 OP_xor OP_lit5 OP_eq ASSERT_TOS_NON0			\
OP_lit3 OP_lit6 OP_le ASSERT_TOS_NON0					\
OP_lit3 OP_lit3 OP_le ASSERT_TOS_NON0					\
OP_lit6 OP_lit3 OP_le ASSERT_TOS_0					\
OP_lit3 OP_lit6 OP_lt ASSERT_TOS_NON0					\
OP_lit3 OP_lit3 OP_lt ASSERT_TOS_0					\
OP_lit6 OP_lit3 OP_lt ASSERT_TOS_0					\
OP_lit3 OP_lit6 OP_ge ASSERT_TOS_0					\
OP_lit3 OP_lit3 OP_ge ASSERT_TOS_NON0					\
OP_lit6 OP_lit3 OP_ge ASSERT_TOS_NON0					\
OP_lit3 OP_lit6 OP_gt ASSERT_TOS_0					\
OP_lit3 OP_lit3 OP_gt ASSERT_TOS_0					\
OP_lit6 OP_lit3 OP_gt ASSERT_TOS_NON0					\
OP_const1s(-6) OP_lit1 OP_shr OP_lit0 OP_gt ASSERT_TOS_NON0		\
OP_const1s(-6) OP_lit1 OP_shra OP_lit0 OP_lt ASSERT_TOS_NON0

#define CFI_ESCAPE_VAL_2(VALUES...) #VALUES
#define CFI_ESCAPE_VAL_1(VALUES...) CFI_ESCAPE_VAL_2(VALUES)
#define CFI_ESCAPE_VAL(VALUES...) CFI_ESCAPE_VAL_1(VALUES)
#define CFI_ESCAPE do { } while (0)
#define CFI_ARCH_PROGRAM OP_nop_termination
#ifdef __GCC_HAVE_DWARF2_CFI_ASM
#if defined (__x86_64__)
#undef CFI_ESCAPE
#undef CFI_ARCH_PROGRAM
#define CFI_ARCH_PROGRAM CFI_PROGRAM OP_lit8 OP_minus OP_nop_termination
unsigned char cfi_arch_program[] = { CFI_ARCH_PROGRAM };
extern char verify_it[sizeof (cfi_arch_program) - 0x80 < 0x3f80 ? 1 : -1];
/* DW_CFA_expression %rip, uleb128(l2-l1), l1: program DW_OP_lit8 DW_OP_minus DW_OP_nop l2: */
#define CFI_ESCAPE \
  asm volatile (".cfi_escape 0x10, 0x10, (%P0&0x7f)+0x80, %P0>>7, " \
		CFI_ESCAPE_VAL (CFI_ARCH_PROGRAM) \
		: : "i" (sizeof (cfi_arch_program)))
#elif defined (__i386__)
#undef CFI_ESCAPE
#undef CFI_ARCH_PROGRAM
#define CFI_ARCH_PROGRAM CFI_PROGRAM OP_lit4 OP_minus OP_nop_termination
unsigned char cfi_arch_program[] = { CFI_ARCH_PROGRAM };
extern char verify_it[sizeof (cfi_arch_program) - 0x80 < 0x3f80 ? 1 : -1];
/* DW_CFA_expression %eip, uleb128(l2-l1), l1: program DW_OP_lit4 DW_OP_minus DW_OP_nop l2: */
#define CFI_ESCAPE \
  asm volatile (".cfi_escape 0x10, 8, (%P0&0x7f)+0x80, %P0>>7, " \
		CFI_ESCAPE_VAL (CFI_ARCH_PROGRAM) \
		: : "i" (sizeof (cfi_arch_program)))
#endif
#endif
static _Unwind_Reason_Code
force_unwind_stop (int version, _Unwind_Action actions,
		   _Unwind_Exception_Class exc_class,
		   struct _Unwind_Exception *exc_obj,
		   struct _Unwind_Context *context,
		   void *stop_parameter)
{
  if (actions & _UA_END_OF_STACK)
    abort ();
  return _URC_NO_REASON;
}

static void force_unwind ()
{
  struct _Unwind_Exception *exc = malloc (sizeof (*exc));
  memset (&exc->exception_class, 0, sizeof (exc->exception_class));
  exc->exception_cleanup = 0;

#ifndef __USING_SJLJ_EXCEPTIONS__
  _Unwind_ForcedUnwind (exc, force_unwind_stop, 0);
#else
  _Unwind_SjLj_ForcedUnwind (exc, force_unwind_stop, 0);
#endif

  abort ();
}

static void handler (void *p __attribute__((unused)))
{
  exit (0);
}

__attribute__((noinline)) static void callme ()
{
  CFI_ESCAPE;
  force_unwind ();
}

__attribute__((noinline)) static void doit ()
{
  char dummy __attribute__((cleanup (handler)));
  callme ();
}

int main()
{ 
  doit ();
  abort ();
}
