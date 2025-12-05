/* Test for #pragma GCC target and push/pop options support in RISC-V */
/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O2" } */

/* Default compilation options - no vector */
void default_func(void) {
#ifdef __riscv_vector
  __builtin_abort();  /* Should not have vector by default */
#endif
}

/* Change target to enable vector */
#pragma GCC push_options
#pragma GCC target("arch=rv64gcv")
void vector_func(void) {
#ifndef __riscv_vector
  __builtin_abort();  /* Should have vector here */
#endif
}
#pragma GCC pop_options

/* Back to default - no vector */
void after_pop_func(void) {
#ifdef __riscv_vector
  __builtin_abort();  /* Should not have vector after pop */
#endif
}

/* Test multiple push/pop levels */
#pragma GCC push_options
#pragma GCC target("arch=rv64gc")
void base_func(void) {
#ifdef __riscv_vector
  __builtin_abort();  /* Should not have vector */
#endif
}

#pragma GCC push_options
#pragma GCC target("arch=rv64gcv")
void nested_vector_func(void) {
#ifndef __riscv_vector
  __builtin_abort();  /* Should have vector here */
#endif
}
#pragma GCC pop_options

void after_nested_pop_func(void) {
#ifdef __riscv_vector
  __builtin_abort();  /* Should not have vector after nested pop */
#endif
}
#pragma GCC pop_options

/* Final function should be back to original default */
void final_func(void) {
#ifdef __riscv_vector
  __builtin_abort();  /* Should not have vector */
#endif
}
