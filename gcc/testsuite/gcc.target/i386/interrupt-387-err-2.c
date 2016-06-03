/* { dg-do compile } */
/* { dg-options "-O2 -mgeneral-regs-only -mno-cld -m80387 -mlong-double-80 -mno-iamcu" } */

void
__attribute__((no_caller_saved_registers))
fn1 (void)
{ /* { dg-message "80387 instructions aren't allowed in function with no_caller_saved_registers attribute" } */
}
