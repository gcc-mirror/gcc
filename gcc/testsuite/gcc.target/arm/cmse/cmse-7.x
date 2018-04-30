int __attribute__ ((cmse_nonsecure_call)) (*bar) (void);

int
foo (int a)
{
  return bar () + a + 1;
}
