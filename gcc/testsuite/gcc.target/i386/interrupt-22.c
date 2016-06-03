/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -mgeneral-regs-only -mno-cld -miamcu -maccumulate-outgoing-args" } */

struct interrupt_frame;

void (*callback) (unsigned int id, unsigned int len)
  __attribute__((no_caller_saved_registers));
unsigned int remaining;

void
__attribute__((no_caller_saved_registers))
handler(void)
{
  while (1) {
    if (remaining) {
      callback(0, 0);
      break;
    }
  }
}

void
__attribute__((interrupt))
my_isr(struct interrupt_frame *frame)
{
  handler();
}

/* { dg-final { scan-assembler-times "\tcld" 1 } } */
