/* { dg-do run } */

volatile int i;

__attribute__((interrupt(1),interrupt(2)))
static void fun12 (void)
{
  __asm goto ("brid %x0" ::: "memory" : l_abort);
  i += 1234;
  return;
  
 l_abort:
  __asm ("%~jmp abort" ::: "memory");
}

extern void isr1 (void) __asm("__vector_1");
extern void isr2 (void) __asm("__vector_2");

int main (void)
{
  __asm ("cli" ::: "memory");
  isr1();
  if (i != 1234)
    __builtin_abort ();

  __asm ("cli" ::: "memory");
  isr2();
  if (i != 2468)
    __builtin_abort ();

  return 0;
}
