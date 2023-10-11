/* { dg-do compile } */
/* { dg-require-effective-target cv_alu } */
/* { dg-options "-march=rv32i -mabi=ilp32" } */

extern int d;

int
foo(int a, int b, int c)
{
    d += __builtin_riscv_cv_alu_slet (a, b); /* { dg-warning "implicit declaration of function" } */
    d += __builtin_riscv_cv_alu_sletu (a, b);  /* { dg-warning "implicit declaration of function" } */
    d += __builtin_riscv_cv_alu_addN (a, b, 31);  /* { dg-warning "implicit declaration of function" } */
    d += __builtin_riscv_cv_alu_addRN (a, b, 31); /* { dg-warning "implicit declaration of function" } */
    d += __builtin_riscv_cv_alu_adduN (a, b, 31); /* { dg-warning "implicit declaration of function" } */
    d += __builtin_riscv_cv_alu_adduRN (a, b, 31); /* { dg-warning "implicit declaration of function" } */
    d += __builtin_riscv_cv_alu_clip (a, 31); /* { dg-warning "implicit declaration of function" } */
    d += __builtin_riscv_cv_alu_clipu (a, 35); /* { dg-warning "implicit declaration of function" } */
    d += __builtin_riscv_cv_alu_extbs (a); /* { dg-warning "implicit declaration of function" } */
    d += __builtin_riscv_cv_alu_extbz (a); /* { dg-warning "implicit declaration of function" } */
    d += __builtin_riscv_cv_alu_exths (a); /* { dg-warning "implicit declaration of function" } */
    d += __builtin_riscv_cv_alu_exthz (a); /* { dg-warning "implicit declaration of function" } */
    d += __builtin_riscv_cv_alu_min (a, b); /* { dg-warning "implicit declaration of function" } */
    d += __builtin_riscv_cv_alu_minu (a, b); /* { dg-warning "implicit declaration of function" } */
    d += __builtin_riscv_cv_alu_max (a, b); /* { dg-warning "implicit declaration of function" } */
    d += __builtin_riscv_cv_alu_maxu (a, b); /* { dg-warning "implicit declaration of function" } */
    d += __builtin_riscv_cv_alu_subN (a, b, 31); /* { dg-warning "implicit declaration of function" } */
    d += __builtin_riscv_cv_alu_subRN (a, b, 31); /* { dg-warning "implicit declaration of function" } */
    d += __builtin_riscv_cv_alu_subuN (a, b, 31); /* { dg-warning "implicit declaration of function" } */
    d += __builtin_riscv_cv_alu_subuRN (a, b, 31); /* { dg-warning "implicit declaration of function" } */

    return d;
}
