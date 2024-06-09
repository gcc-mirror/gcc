/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O1" } */

__attribute__((riscv_vector_cc)) int var; /* { dg-warning "'riscv_vector_cc' attribute only applies to function types" } */
[[riscv::vector_cc]] int var1; /* { dg-warning "'vector_cc' attribute only applies to function types" } */

void __attribute__((riscv_vector_cc)) func();
void __attribute__((riscv_vector_cc(1))) func_invalid(); /* { dg-error "wrong number of arguments specified for 'riscv_vector_cc' attribute" } */

void test_no_attribute(int);
void __attribute__((riscv_vector_cc)) test_no_attribute(int x) { }
