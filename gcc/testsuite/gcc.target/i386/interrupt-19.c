/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -mgeneral-regs-only -mno-cld -mno-iamcu" } */

struct ret
{
  int i[8];
};

extern struct ret bar (void);

void
 __attribute__ ((interrupt))
fn (void *frame)
{
  bar ();
} /* { dg-message "sorry, unimplemented: Dynamic Realign Argument Pointer" } */
