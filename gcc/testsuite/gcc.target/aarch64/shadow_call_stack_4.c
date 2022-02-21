/* Testing the disable of shadow call stack.  */
/* scs_push: str x30, [x18], #8 */
/* scs_pop: ldr x30, [x18, #-8]! */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-omit-frame-pointer -fsanitize=shadow-call-stack -ffixed-x18 -fno-exceptions" } */

int foo (int);

/* function disable shadow call stack.  */
int __attribute__((no_sanitize("shadow-call-stack"))) func1 (void)
{
  asm volatile ("":::"x30");

  return 0;
}

/* { dg-final { scan-assembler-not {str\tx30, \[x18\], #?8} } } */
/* { dg-final { scan-assembler-not {ldr\tx30, \[x18, #?-8\]!} } } */
/* { dg-final { scan-assembler-times {stp\tx29, x30, \[sp, -[0-9]+\]!} 1 } } */
/* { dg-final { scan-assembler-times {ldp\tx29, x30, \[sp\], [0-9]+} 1 } } */
