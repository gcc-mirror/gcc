/* { dg-do run { target lp64 } } */
/* { dg-options "-O2" } */

__attribute__((noipa)) int
foo (void)
{
  int a = 1;
  int b = 2;
  int c = 3;
  int d = 4;
  int e = 5;
  int f = 6;
  int g = 7;
  int h = 8;
  int i = 9;
  int j = 10;
  int k = 11;
  int l = 12;
  int m = 13;
  int n = 14;
  asm volatile ("" : "+g" (a), "+g" (b), "+g" (c), "+g" (d), "+g" (e));
  asm volatile ("" : "+g" (f), "+g" (g), "+g" (h), "+g" (i), "+g" (j));
  asm volatile ("" : "+g" (k), "+g" (l), "+g" (m), "+g" (n));
  asm volatile ("{pushq %%rax; pushq %%rax; popq %%rax; popq %%rax"
		"|push rax;push rax;pop rax;pop rax}"
		: : : "ax", "si", "di", "r10", "r11", "redzone");
  asm volatile ("" : "+g" (a), "+g" (b), "+g" (c), "+g" (d), "+g" (e));
  asm volatile ("" : "+g" (f), "+g" (g), "+g" (h), "+g" (i), "+g" (j));
  asm volatile ("" : "+g" (k), "+g" (l), "+g" (m), "+g" (n));
  return a + b + c + d + e + f + g + h + i + j + k + l + m + n;
}

int
main ()
{
  if (foo () != 105)
    __builtin_abort ();
}
