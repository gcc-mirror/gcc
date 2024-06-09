/* PR middle-end/90348 */

void __attribute__ ((noipa))
set_one (unsigned char *ptr)
{
  *ptr = 1;
}

void __attribute__ ((noipa))
check_zero (unsigned char const *in, unsigned int len)
{
  for (unsigned int i = 0; i < len; ++i)
    if (in[i] != 0)
      __builtin_abort ();
}

static void
set_one_on_stack (void)
{
  unsigned char buf[1];
  set_one (buf);
}

int
main ()
{
  for (int i = 0; i <= 4; ++i)
    {
      unsigned char in[4];
      for (int j = 0; j < i; ++j)
	{
	  in[j] = 0;
	  set_one_on_stack ();
	}
      check_zero (in, i);
    }
  return 0;
}
