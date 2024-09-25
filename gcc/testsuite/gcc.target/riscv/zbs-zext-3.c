/* { dg-do compile } */
/* { dg-options "-march=rv64gcb -mabi=lp64d" { target { rv64 } } } */
/* { dg-options "-march=rv32gcb -mabi=ilp32" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

/* We need to adjust the constant so this works for rv32 and rv64.  */
#if __riscv_xlen == 32
#define ONE 1U
#else
#define ONE 1ULL
#endif

void add_to_hard_reg_set(long long *a, unsigned int count) {
  int i = 0;
  while(i++ < count)
    *a |= (1U << i);
}

void remove_from_hard_reg_set(long long *a, unsigned int count) {
  int i = 0;
  while(i++ < count)
    *a &= ~(ONE << i);
}


/* { dg-final { scan-assembler-not "and\t" } } */
/* { dg-final { scan-assembler-not "andn\t" } } */
