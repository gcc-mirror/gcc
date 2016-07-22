/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -mgeneral-regs-only -mno-cld -miamcu -maccumulate-outgoing-args" } */

struct interrupt_frame;

extern void callback0 (unsigned int id, unsigned int len)
  __attribute__((no_caller_saved_registers));
extern void callback1 (unsigned int id, unsigned int len)
  __attribute__((no_caller_saved_registers));
extern void callback2 (unsigned int id, unsigned int len)
  __attribute__((no_caller_saved_registers));

typedef void (*callback_t) (unsigned int id, unsigned int len)
  __attribute__((no_caller_saved_registers));

callback_t callback[] =
{
  callback0,
  callback1,
  callback2,
};

unsigned int remaining;

void
__attribute__((no_caller_saved_registers))
handler(int uart)
{
  while (1) {
    if (remaining) {
      callback[uart](0, 0);
      break;
    }
  }
}

int uart;

void
__attribute__((interrupt))
my_isr(struct interrupt_frame *frame)
{
  handler(uart);
}

/* { dg-final { scan-assembler-times "\tcld" 1 } } */
