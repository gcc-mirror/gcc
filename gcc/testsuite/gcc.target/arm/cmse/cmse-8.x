int __attribute__ ((cmse_nonsecure_call)) (*bar) (double);

int
foo (int a)
{
  return bar (2.0) + a + 1;
}
