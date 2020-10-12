/* Based on execute/simd-1.c, modified by joern.rennecke@st.com to
   trigger a reload bug.  Verified for gcc mainline from 20050722 13:00 UTC
   for sh-elf -m4 -O2.  */
/* { dg-options "-Wno-psabi -fwrapv" } */
/* { dg-add-options stack_size } */

#ifndef STACK_SIZE
#define STACK_SIZE (256*1024)
#endif

extern void abort (void);
extern void exit (int);

typedef struct { char c[STACK_SIZE/2]; } big_t;

typedef int __attribute__((mode(SI))) __attribute__((vector_size (8))) vecint;
typedef int __attribute__((mode(SI))) siint;

vecint i = { 150, 100 };
vecint j = { 10, 13 };
vecint k;

union {
  vecint v;
  siint i[2];
} res;

void
verify (siint a1, siint a2, siint b1, siint b2, big_t big)
{
  if (a1 != b1
      || a2 != b2)
    abort ();
}

int
main ()
{
  big_t big;
  vecint k0, k1, k2, k3, k4, k5, k6, k7;

  k0 = i + j;
  res.v = k0;

  verify (res.i[0], res.i[1], 160, 113, big);

  k1 = i * j;
  res.v = k1;

  verify (res.i[0], res.i[1], 1500, 1300, big);

  k2 = i / j;
/* This is the observed failure - reload 0 has the wrong type and thus the
   conflict with reload 1 is missed:

(insn:HI 94 92 96 1 pr23135.c:46 (parallel [
            (set (subreg:SI (reg:DI 253) 0)
                (div:SI (reg:SI 4 r4)
                    (reg:SI 5 r5)))
            (clobber (reg:SI 146 pr))
            (clobber (reg:DF 64 fr0))
            (clobber (reg:DF 66 fr2))
            (use (reg:PSI 151 ))
            (use (reg/f:SI 256))
        ]) 60 {divsi3_i4} (insn_list:REG_DEP_TRUE 90 (insn_list:REG_DEP_TRUE 89
(insn_list:REG_DEP_TRUE 42 (insn_list:REG_DEP_TRUE 83 (insn_list:REG_DEP_TRUE 92
 (insn_list:REG_DEP_TRUE 91 (nil)))))))
    (expr_list:REG_DEAD (reg:SI 4 r4)
        (expr_list:REG_DEAD (reg:SI 5 r5)
            (expr_list:REG_UNUSED (reg:DF 66 fr2)
                (expr_list:REG_UNUSED (reg:DF 64 fr0)
                    (expr_list:REG_UNUSED (reg:SI 146 pr)
                        (insn_list:REG_RETVAL 91 (nil))))))))

Reloads for insn # 94
Reload 0: reload_in (SI) = (plus:SI (reg/f:SI 14 r14)
                                                    (const_int 64 [0x40]))
        GENERAL_REGS, RELOAD_FOR_OUTADDR_ADDRESS (opnum = 0)
        reload_in_reg: (plus:SI (reg/f:SI 14 r14)
                                                    (const_int 64 [0x40]))
        reload_reg_rtx: (reg:SI 3 r3)
Reload 1: GENERAL_REGS, RELOAD_FOR_OUTPUT_ADDRESS (opnum = 0), can't combine, se
condary_reload_p
        reload_reg_rtx: (reg:SI 3 r3)
Reload 2: reload_out (SI) = (mem:SI (plus:SI (plus:SI (reg/f:SI 14 r14)
                                                            (const_int 64 [0x40]))
                                                        (const_int 28 [0x1c])) [ 16 S8 A32])
        FPUL_REGS, RELOAD_FOR_OUTPUT (opnum = 0)
        reload_out_reg: (subreg:SI (reg:DI 253) 0)
        reload_reg_rtx: (reg:SI 150 fpul)
        secondary_out_reload = 1

Reload 3: reload_in (SI) = (symbol_ref:SI ("__sdivsi3_i4") [flags 0x1])
        GENERAL_REGS, RELOAD_FOR_INPUT (opnum = 1), can't combine
        reload_in_reg: (reg/f:SI 256)
        reload_reg_rtx: (reg:SI 3 r3)
  */


  res.v = k2;

  verify (res.i[0], res.i[1], 15, 7, big);

  k3 = i & j;
  res.v = k3;

  verify (res.i[0], res.i[1], 2, 4, big);

  k4 = i | j;
  res.v = k4;

  verify (res.i[0], res.i[1], 158, 109, big);

  k5 = i ^ j;
  res.v = k5;

  verify (res.i[0], res.i[1], 156, 105, big);

  k6 = -i;
  res.v = k6;
  verify (res.i[0], res.i[1], -150, -100, big);

  k7 = ~i;
  res.v = k7;
  verify (res.i[0], res.i[1], -151, -101, big);

  k = k0 + k1 + k3 + k4 + k5 + k6 + k7;
  res.v = k;
  verify (res.i[0], res.i[1], 1675, 1430, big);

  k = k0 * k1 * k3 * k4 * k5 * k6 * k7;
  res.v = k;
  verify (res.i[0], res.i[1], 1456467968, -1579586240, big);

  k = k0 / k1 / k2 / k3 / k4 / k5 / k6 / k7;
  res.v = k;
  verify (res.i[0], res.i[1], 0, 0, big);

  exit (0);
}
