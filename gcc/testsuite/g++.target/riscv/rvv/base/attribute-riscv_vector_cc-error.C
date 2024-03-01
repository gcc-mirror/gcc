/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O1" } */

[[riscv::vector_cc]] void foo();// For C++11 and C23

[[riscv::vector_cc]] int var; /* { dg-warning "'vector_cc' attribute only applies to function types" } */

void __attribute__((riscv_vector_cc)) func();
void __attribute__((riscv_vector_cc(1))) func_invalid(); /* { dg-error "wrong number of arguments specified for 'riscv_vector_cc' attribute" } */

void test_no_attribute(int);
void  __attribute__((riscv_vector_cc)) test_no_attribute(int x) { }

class test_cc {
   __attribute__((riscv_vector_cc)) void member_func();
};

void test_lambda() {
   __attribute__((riscv_vector_cc)) auto lambda = []() { /* { dg-warning "'riscv_vector_cc' attribute only applies to function types" } */
  };
}
