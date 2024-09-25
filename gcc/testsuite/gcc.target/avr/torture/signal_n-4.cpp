/* { dg-do run } */

class IRQS
{
public:
  void test (int x) const
  {
    if (i != x)
      __builtin_abort ();
  }

private:
  static volatile int i;

  __attribute__((signal(1,2)))
  static void fun12 ()
  {
    i += 1234;
  }
};

extern void isr1 () __asm("__vector_1");
extern void isr2 () __asm("__vector_2");
extern void isr3 () __asm("__vector_3");

IRQS irqs;

volatile int IRQS::i;

namespace
{
  int j;
  __attribute__((signal(3)))
  void handle3 ()
  {
    j = 444;
  }
}

int main (void)
{
  isr1();
  irqs.test (1234);

  isr2();
  irqs.test (2468);

  isr3 ();
  if (j != 444)
  __builtin_abort();
  
  return 0;
}
