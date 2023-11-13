extern int (*gp)(const char*);

int
g (const char* d)
{
  __builtin_printf ("g");
  return 0;
}

void
f (void)
{
  int errcnt=0;

  if (gp != g)
    {
      __builtin_printf ("f");
      errcnt++;
    }
}
