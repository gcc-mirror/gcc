int __attribute__ ((cmse_nonsecure_call)) (*bar) (float, double);

int
foo (int a)
{
  return bar (3.0f, 2.0) + a + 1;
}
