typedef void tfoo (int *);
tfoo *getfoo (void);

void
bar (int *i)
{
  (*i)--;
}

int
main ()
{
  int i = 1;
  getfoo ()(&i);
  if (i)
    __builtin_abort ();
  return 0;
}

