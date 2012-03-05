/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O1" } */
static int global = 0;

__attribute__((transaction_pure))
static inline void purefunc()
{
  global++;
}

__attribute__((transaction_safe))
void f();

void push()
{
  __transaction_atomic {
        f();
    purefunc();
  }
}

/* { dg-final { scan-assembler-not "_ITM_RfWU4" } } */
