struct usc_bigstack_t {};

void
usc_recressive_func(int cnt, int max, struct usc_bigstack_t bstack)
{
  usc_recressive_func(cnt+1, max, bstack);
}
