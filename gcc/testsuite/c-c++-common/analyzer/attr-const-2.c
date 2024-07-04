extern int const_p (int) __attribute__((const));
extern void do_stuff (void);

void test (int a)
{
  void *p;
  if (const_p (a))
    {
      p = __builtin_malloc (1024);
      if (!p)
	return;
    }
  do_stuff ();
  if (const_p (a))
    __builtin_free (p); /* { dg-bogus "uninit" } */
}
