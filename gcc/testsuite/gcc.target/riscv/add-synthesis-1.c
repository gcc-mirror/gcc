/* { dg-options "-march=rv32gcb -mabi=ilp32d" { target { rv32 } } } */
/* { dg-options "-march=rv64gcb -mabi=lp64d" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */



#if  __riscv_xlen == 64
#define TYPE long
#else
#define TYPE int
#endif

#define T(C)  TYPE foo_##C (TYPE x) { return x + C; }
#define TM(C)  TYPE foo_M##C (TYPE x) { return x + -C; }

/* These cases were selected because they all can be synthesized
   at expansion time without synthesizing the constant directly.

   That makes the assembler scan testing simpler.  I've verified
   by hand that cases that should synthesize the constant do in
   fact still generate code that way.  */
T (2050)
T (4094)
T (4100)
T (8200)

TM (2049)
TM (4094)
TM (4100)
TM (8200)

#if  __riscv_xlen == 64
TM (0x200000000)
#endif

/* We have 4/5 tests which should use shNadd insns and 4
   which used paired addi insns.  */
/* { dg-final { scan-assembler-times "sh.add\t" 4 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "sh.add\t" 5 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "addi\t" 8 } } */
