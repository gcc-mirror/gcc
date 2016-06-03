/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2  -mgeneral-regs-only -mno-cld -miamcu -maccumulate-outgoing-args" } */

struct interrupt_frame;

void (*callback[1])(unsigned int id, unsigned int len);
unsigned int remaining;

void
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
