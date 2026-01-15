/* { dg-do compile { target aarch64*-*-* arm*-*-* i?86-*-* powerpc*-*-* pru*-*-* riscv*-*-* s390*-*-* x86_64-*-* } } */

#if defined (__aarch64__)
# define GPR1_RAW "x0"
# define GPR2 "{x1}"
# define GPR3 "{x2}"
# define INVALID_GPR_A "{x31}"
#elif defined (__arm__)
# define GPR1_RAW "r0"
# define GPR2 "{r1}"
# define GPR3 "{r2}"
# define INVALID_GPR_A "{r16}"
#elif defined (__i386__)
# define GPR1_RAW "%eax"
# define GPR2 "{%ebx}"
# define GPR3 "{%edx}"
# define INVALID_GPR_A "{%eex}"
#elif defined (__powerpc__) || defined (__POWERPC__)
# define GPR1_RAW "r4"
# define GPR2 "{r5}"
# define GPR3 "{r6}"
# define INVALID_GPR_A "{r33}"
#elif defined (__PRU__)
# define GPR1_RAW "r20"
# define GPR2 "{r21}"
# define GPR3 "{r22}"
# define INVALID_GPR_A "{r34}"
#elif defined (__riscv)
# define GPR1_RAW "t4"
# define GPR2 "{t5}"
# define GPR3 "{t6}"
# define INVALID_GPR_A "{t7}"
#elif defined (__s390__)
# define GPR1_RAW "r4"
# define GPR2 "{r5}"
# define GPR3 "{r6}"
# define INVALID_GPR_A "{r17}"
#elif defined (__x86_64__)
# define GPR1_RAW "rax"
# define GPR2 "{rbx}"
# define GPR3 "{rcx}"
# define INVALID_GPR_A "{rex}"
#endif

#define GPR1 "{"GPR1_RAW"}"
#define INVALID_GPR_B "{"GPR1_RAW

struct { int a[128]; } s = {0};

void
test (void)
{
  int x, y;
  register int gpr1 __asm__ (GPR1_RAW) = 0;

  __asm__ ("" :: "{}" (42)); /* { dg-error "invalid input constraint: \{\}" } */
  __asm__ ("" :: INVALID_GPR_A (42)); /* { dg-error "invalid input constraint" } */
  __asm__ ("" :: INVALID_GPR_B (42)); /* { dg-error "invalid input constraint" } */

  __asm__ ("" :: GPR1 (s)); /* { dg-error "data type isn't suitable for register .* of operand 0" } */

  __asm__ ("" :: "r" (gpr1), GPR1 (42)); /* { dg-error "multiple inputs to hard register" } */
  __asm__ ("" :: GPR1 (42), "r" (gpr1)); /* { dg-error "multiple inputs to hard register" } */
  __asm__ ("" :: GPR1 (42), GPR1 (42)); /* { dg-error "multiple inputs to hard register" } */
  __asm__ ("" :: GPR1","GPR2 (42), GPR2","GPR3 (42));
  __asm__ ("" :: GPR1","GPR2 (42), GPR3","GPR2 (42)); /* { dg-error "multiple inputs to hard register" } */
  __asm__ ("" :: GPR1","GPR2 (42), GPR1","GPR3 (42)); /* { dg-error "multiple inputs to hard register" } */
  __asm__ ("" : "+"GPR1 (x), "="GPR1 (y)); /* { dg-error "multiple outputs to hard register" } */
  __asm__ ("" : "="GPR1 (y) : GPR1 (42), "0" (42)); /* { dg-error "multiple inputs to hard register" } */
  __asm__ ("" : "+"GPR1 (x) : GPR1 (42)); /* { dg-error "multiple inputs to hard register" } */

  __asm__ ("" : "="GPR1 (gpr1));
  __asm__ ("" : "="GPR2 (gpr1)); /* { dg-error "constraint and register 'asm' for output operand 0 are unsatisfiable" } */
  __asm__ ("" :: GPR2 (gpr1)); /* { dg-error "constraint and register 'asm' for input operand 0 are unsatisfiable" } */
  __asm__ ("" : "="GPR1 (x) : "0" (gpr1));
  __asm__ ("" : "="GPR2 (x) : "0" (gpr1)); /* { dg-error "constraint and register 'asm' for input operand 0 are unsatisfiable" } */

  __asm__ ("" : "=&"GPR1 (x) : "0" (gpr1));
  __asm__ ("" : "=&"GPR1 (x) : "0" (42));
  __asm__ ("" : "=&"GPR2","GPR1 (x) : "r,"GPR1 (42));
  __asm__ ("" : "="GPR2",&"GPR1 (x) : "r,"GPR1 (42)); /* { dg-error "invalid hard register usage between earlyclobber operand and input operand" } */
  __asm__ ("" : "=&"GPR1 (x) : GPR1 (42)); /* { dg-error "invalid hard register usage between earlyclobber operand and input operand" } */
  __asm__ ("" : "=&"GPR2","GPR1 (x) : "r,r" (gpr1));
  __asm__ ("" : "="GPR2",&"GPR1 (x) : "r,r" (gpr1)); /* { dg-error "invalid hard register usage between earlyclobber operand and input operand" } */
  __asm__ ("" : "=&r" (gpr1) : GPR1 (42)); /* { dg-error "invalid hard register usage between earlyclobber operand and input operand" } */
  __asm__ ("" : "=&"GPR1 (x),  "=r" (y) : "1" (gpr1)); /* { dg-error "invalid hard register usage between earlyclobber operand and input operand" } */
}
