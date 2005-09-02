int check_fa_work (const char *, const char *) __attribute__((noinline));
int check_fa_mid (const char *) __attribute__((noinline));
int check_fa (char *) __attribute__((noinline));
int how_much (void) __attribute__((noinline));

int check_fa_work (const char *c, const char *f)
{
  const char d = 0;

  if (c >= &d)
    return c >= f && f >= &d;
  else
    return c <= f && f <= &d;
}

int check_fa_mid (const char *c)
{
  const char *f = __builtin_frame_address (0);

  /* Prevent a tail call to check_fa_work, eliding the current stack frame.  */
  return check_fa_work (c, f) != 0;
}

int check_fa (char *unused)
{
  const char c = 0;

  return check_fa_mid (&c);
}

int how_much (void)
{
	return 8;
}

int main (void)
{
  char *unused = __builtin_alloca (how_much ());

  if (!check_fa(unused))
    abort();
  return 0;
}
