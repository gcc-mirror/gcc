extern void exit (int);

static inline int
foo (void)
{
#ifdef __OPTIMIZE__
  extern int undefined_reference;
  return undefined_reference;
#else
  return 0;
#endif
}

static inline int
bar (void)
{
  if (foo == foo)
    return 1;
  else
    return foo ();
}

int main (void)
{
  exit (0);
}
