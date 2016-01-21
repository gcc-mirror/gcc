_Noreturn void fn1 (int) __attribute__((__visibility__("hidden")));

void
fn2 (void *p1)
{
  int a[7];
  float *b;
  int c, n;

  if (c != p1) /* { dg-warning "comparison between pointer and integer" } */
    fn1 (1);

  n = 0;
  for (; c; n++)
    {
      int d;
      if (a[n] != d)
	fn1(n);
    }

  b = p1;

  while (1)
    {
      *b = 3.40282347e38f;
      if (a[0])
	return;
    }
}
