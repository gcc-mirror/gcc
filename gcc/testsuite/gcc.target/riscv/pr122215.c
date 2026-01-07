/* { dg-do compile } */
/* { dg-require-effective-target valgrind } */
/* { dg-additional-files "sparseset.supp" } */
/* { dg-options "-wrapper valgrind,-q,--exit-on-first-error=yes,--error-exitcode=1,--suppressions=${srcdir}/sparseset.supp" } */

typedef signed int int32_t;
typedef signed long int int64_t;

int64_t dual_reg_insn(int64_t x) {
    int64_t res;
    int64_t zero = 0;
    asm ("some_custom_insn %0,%1,%2" : "=R" (res) : "R" (x), "R" (zero));
    return res;
}

int32_t single_reg_insn(int32_t x) {
    int32_t res;
    int32_t zero = 0;
    asm ("some_custom_insn %0,%1,%2" : "=r" (res) : "r" (x), "r" (zero));
    return res;
}

int32_t single_reg_insn_explicit_zero(int32_t x) {
    int32_t res;
    asm ("some_custom_insn %0,%1,%2" : "=r" (res) : "r" (x), "r" (0));
    return res;
}

int64_t dual_reg_insn2(int64_t x) {
    int64_t res;
    int64_t zero = 0;
    asm ("some_custom_insn %0,%1,%2" : "=R" (res) : "R" (x), "R" (zero));
    return res;
    /* This function is IDENTICAL to dual_reg_insn,
     * but for some obscure reason (alignment?)
     * it decides to use sX registers instead of aX to store zero,
     * resulting in a much larger code since it needs to use the stack.
     * THIS ONLY HAPPENS SOMETIMES!
     */
}

int64_t dual_reg_insn_explicit_zero(int64_t x) {
    int64_t res;
    asm ("some_custom_insn %0,%1,%2" : "=R" (res) : "R" (x), "R" (0LL));
    return res;
}
