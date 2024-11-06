/* { dg-do compile } */
/* { dg-additional-options "-std=gnu17" } */

typedef int UInt;
UInt skeletal_RI5_instr;
__attribute__((__noreturn__)) void vex_assert_fail();
typedef struct {
  union {
    struct {
      UInt imm5;
    } I5;
  } ARMri5;
} ARMRI5;
typedef enum { ARMin_Alu, ARMin_Shift } ARMInstrTag;
void iregEnc();
static UInt skeletal_RI5(ARMRI5 *ri) {
  UInt imm5 = ri->ARMri5.I5.imm5;
  __builtin_expect(imm5, 1) ?: vex_assert_fail();
  iregEnc(ri->ARMri5);
  return skeletal_RI5_instr;
}
ARMInstrTag emit_ARMInstr_i_0;
void *emit_ARMInstr_disp_cp_chain_me_to_slowEP() {
  switch (emit_ARMInstr_i_0) {
  case ARMin_Alu:
    UInt instr, subopc;
    UInt rD, rN;
    goto bad;
    instr |= subopc | rN;
  case ARMin_Shift:
    rD = 0;
    UInt rM = 0;
    ARMRI5 argR;
    instr = skeletal_RI5(&argR);
    instr |= rD | rM;
    goto done;
  }
bad:
done:
  return 0;
}
