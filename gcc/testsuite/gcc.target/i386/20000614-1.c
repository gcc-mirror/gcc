/* { dg-do run } */
/* { dg-options "-O2" } */

void bar(char *p)
{
}

static inline void foo (unsigned long base, unsigned char val)
{
  val ^= (1<<2);
  bar (val & (1<<5) ? "1" : "2");
  bar (val & (1<<4) ? "1" : "2");
  bar (val & (1<<3) ? "1" : "2");
  bar (val & (1<<2) ? "1" : "2");
  bar (val & (1<<1) ? "1" : "2");
  bar (val & (1<<0) ? "1" : "2");
  asm volatile ("": :"a" (val), "d" (base));
}

int main (void)
{
  foo (23, 1);
  return 0;
}
