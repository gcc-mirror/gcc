/* PR target/105116 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-split-wide-types" } */

/* Wide GNU vectors pinned to SVE z registers and then scalarised by vector
   lowering used to ICE in lra_split_hard_reg_for ("unable to find a register
   to spill"): LRA reloaded a non-representable scalar subreg of the OImode
   register pair into GENERAL_REGS, which cannot hold OImode.  Integer vector
   divide and modulo have no Advanced SIMD form, so they are still scalarised
   and exercise the reload path.  */

typedef signed char vnx16qi __attribute__((vector_size (32)));

#define TEST(NAME, OP)						\
  void								\
  NAME (vnx16qi *x, vnx16qi y, vnx16qi z)			\
  {								\
    register vnx16qi dst  asm ("z0");				\
    register vnx16qi src1 asm ("z2");				\
    register vnx16qi src2 asm ("z4");				\
    dst = *x;							\
    src1 = y;							\
    src2 = z;							\
    asm volatile ("" :: "w" (dst), "w" (src1), "w" (src2));	\
    dst = src2 - (OP);						\
    asm volatile ("" :: "w" (dst));				\
    *x = dst;							\
  }

TEST (vmul_vnx16qi, dst * src1)
TEST (vdiv_vnx16qi, dst / src1)
TEST (vmod_vnx16qi, dst % src1)
