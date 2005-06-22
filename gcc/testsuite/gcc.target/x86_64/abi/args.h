#ifndef INCLUDED_ARGS_H
#define INCLUDED_ARGS_H

#include <string.h>

/* This defines the calling sequences for integers and floats.  */
#define I0 rdi
#define I1 rsi
#define I2 rdx
#define I3 rcx
#define I4 r8
#define I5 r9
#define F0 xmm0
#define F1 xmm1
#define F2 xmm2
#define F3 xmm3
#define F4 xmm4
#define F5 xmm5
#define F6 xmm6
#define F7 xmm7

typedef union {
  float _float[4];
  double _double[2];
  long _long[2];
  int _int[4];
  unsigned long _ulong[2];
} XMM_T;

typedef union {
  float _float;
  double _double;
  ldouble _ldouble;
  ulong _ulong[2];
} X87_T;
extern void (*callthis)(void);
extern unsigned long rax,rbx,rcx,rdx,rsi,rdi,rsp,rbp,r8,r9,r10,r11,r12,r13,r14,r15;
XMM_T xmm_regs[16];
X87_T x87_regs[8];
extern volatile unsigned long volatile_var;
extern void snapshot (void);
extern void snapshot_ret (void);
#define WRAP_CALL(N) \
  (callthis = (void (*)()) (N), (typeof (&N)) snapshot)
#define WRAP_RET(N) \
  (callthis = (void (*)()) (N), (typeof (&N)) snapshot_ret)

/* Clear all integer registers.  */
#define clear_int_hardware_registers \
  asm __volatile__ ("xor %%rax, %%rax\n\t" \
		    "xor %%rbx, %%rbx\n\t" \
		    "xor %%rcx, %%rcx\n\t" \
		    "xor %%rdx, %%rdx\n\t" \
		    "xor %%rsi, %%rsi\n\t" \
		    "xor %%rdi, %%rdi\n\t" \
		    "xor %%r8, %%r8\n\t" \
		    "xor %%r9, %%r9\n\t" \
		    "xor %%r10, %%r10\n\t" \
		    "xor %%r11, %%r11\n\t" \
		    "xor %%r12, %%r12\n\t" \
		    "xor %%r13, %%r13\n\t" \
		    "xor %%r14, %%r14\n\t" \
		    "xor %%r15, %%r15\n\t" \
		    ::: "rax", "rbx", "rcx", "rdx", "rsi", "rdi", "r8", \
		    "r9", "r10", "r11", "r12", "r13", "r14", "r15");

/* This is the list of registers available for passing arguments. Not all of
   these are used or even really available.  */
struct IntegerRegisters
{
  unsigned long rax, rbx, rcx, rdx, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15;
};
struct FloatRegisters
{
  double mm0, mm1, mm2, mm3, mm4, mm5, mm6, mm7;
  ldouble st0, st1, st2, st3, st4, st5, st6, st7;
  XMM_T xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7, xmm8, xmm9,
        xmm10, xmm11, xmm12, xmm13, xmm14, xmm15;
};

/* Implemented in scalarargs.c  */
extern struct IntegerRegisters iregs;
extern struct FloatRegisters fregs;
extern unsigned int num_iregs, num_fregs;

#define check_int_arguments do { \
  assert (num_iregs <= 0 || iregs.I0 == I0); \
  assert (num_iregs <= 1 || iregs.I1 == I1); \
  assert (num_iregs <= 2 || iregs.I2 == I2); \
  assert (num_iregs <= 3 || iregs.I3 == I3); \
  assert (num_iregs <= 4 || iregs.I4 == I4); \
  assert (num_iregs <= 5 || iregs.I5 == I5); \
  } while (0)

#define check_char_arguments check_int_arguments
#define check_short_arguments check_int_arguments
#define check_long_arguments check_int_arguments

/* Clear register struct.  */
#define clear_struct_registers \
  rax = rbx = rcx = rdx = rdi = rsi = rbp = rsp \
    = r8 = r9 = r10 = r11 = r12 = r13 = r14 = r15 = 0; \
  memset (&iregs, 0, sizeof (iregs)); \
  memset (&fregs, 0, sizeof (fregs)); \
  memset (xmm_regs, 0, sizeof (xmm_regs)); \
  memset (x87_regs, 0, sizeof (x87_regs));

/* Clear both hardware and register structs for integers.  */
#define clear_int_registers \
  clear_struct_registers \
  clear_int_hardware_registers

/* TODO: Do the checking.  */
#define check_f_arguments(T) { \
  assert (num_fregs <= 0 || fregs.xmm0._ ## T [0] == xmm_regs[0]._ ## T [0]); \
  assert (num_fregs <= 1 || fregs.xmm1._ ## T [0] == xmm_regs[1]._ ## T [0]); \
  assert (num_fregs <= 2 || fregs.xmm2._ ## T [0] == xmm_regs[2]._ ## T [0]); \
  assert (num_fregs <= 3 || fregs.xmm3._ ## T [0] == xmm_regs[3]._ ## T [0]); \
  assert (num_fregs <= 4 || fregs.xmm4._ ## T [0] == xmm_regs[4]._ ## T [0]); \
  assert (num_fregs <= 5 || fregs.xmm5._ ## T [0] == xmm_regs[5]._ ## T [0]); \
  assert (num_fregs <= 6 || fregs.xmm6._ ## T [0] == xmm_regs[6]._ ## T [0]); \
  assert (num_fregs <= 7 || fregs.xmm7._ ## T [0] == xmm_regs[7]._ ## T [0]); \
  } while (0)

#define check_float_arguments check_f_arguments(float)
#define check_double_arguments check_f_arguments(double)

/* ldoubles are not passed in registers */
#define check_ldouble_arguments

/* TODO: Do the clearing.  */
#define clear_float_hardware_registers
#define clear_x87_hardware_registers

#define clear_float_registers \
  clear_struct_registers \
  clear_float_hardware_registers

#define clear_x87_registers \
  clear_struct_registers \
  clear_x87_hardware_registers


#endif /* INCLUDED_ARGS_H  */
