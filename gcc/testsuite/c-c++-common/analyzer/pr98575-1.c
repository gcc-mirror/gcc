/* A malloced pointer that's written to a global pointer shouldn't be
   reported as leaking, even if an unknown function has been called
   (PR analyzer/98575).  */

void **g;

extern void unknown_fn (void);

/* Without a call to unknown_fn.  */

int test_1 (void)
{
  void *p;
  p = __builtin_malloc(1024);
  *g = p;
  return 0;
}

/* With a call to unknown_fn in various places.  */

int test_2 (void)
{
  void *p;
  unknown_fn ();
  p = __builtin_malloc(1024);
  *g = p;
  return 0;
}

int test_3 (void)
{
  void *p;
  p = __builtin_malloc(1024);
  unknown_fn ();
  *g = p;
  return 0;
}

int test_4 (void)
{
  void *p;
  p = __builtin_malloc(1024);
  *g = p;
  unknown_fn ();
  return 0;
}
