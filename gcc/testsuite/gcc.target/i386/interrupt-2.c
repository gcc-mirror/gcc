/* { dg-do compile } */
/* { dg-options "-O2 -mgeneral-regs-only -mno-cld -Wall -g" } */

void
__attribute__((interrupt))
fn (void *frame)
{
}

typedef void (*fn_t) (void *) __attribute__((interrupt));

fn_t fns[] =
{
  fn,
};

/* { dg-final { scan-assembler-not "add(l|q)\[\\t \]*\\$\[0-9\]*,\[\\t \]*%\[re\]?sp" } } */
/* { dg-final { scan-assembler-times "iret" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "iretq" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "\tcld" } } */
