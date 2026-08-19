/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-O2 -masm=att -mno-avx -mno-mmx -mno-80387 -msse" } */

extern void callee_clobbered () [[gnu::preserve_none]];
extern void callee_preserved () [[gnu::no_caller_saved_registers]];
extern void callee_sysv () [[gnu::sysv_abi]];
extern void callee_ms () [[gnu::ms_abi]];

#define TEST(PREFIX, ATTR) \
  void PREFIX##_clobbered () [[ATTR]] { callee_clobbered (); } \
  void PREFIX##_preserved () [[ATTR]] { callee_preserved (); } \
  void PREFIX##_sysv () [[ATTR]] { callee_sysv (); } \
  void PREFIX##_ms () [[ATTR]] { callee_ms (); }

TEST (clobbered, gnu::preserve_none)
TEST (preserved, gnu::no_caller_saved_registers)
TEST (sysv, gnu::sysv_abi)
TEST (ms, gnu::ms_abi)

/* { dg-final { scan-assembler-not "%st" } } */
/* { dg-final { scan-assembler "%xmm" } } */
