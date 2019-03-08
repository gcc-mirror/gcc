/* { dg-do compile } */
/* { dg-options "-O2 -mgeneral-regs-only -mmmx -mno-cld -mno-iamcu" } */

void
__attribute__((no_caller_saved_registers))
fn1 (void)
{ /* { dg-message "MMX/3Dnow instructions aren't allowed in a function with the 'no_caller_saved_registers' attribute" } */
}
