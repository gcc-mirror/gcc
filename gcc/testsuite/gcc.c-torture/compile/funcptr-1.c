extern int (*gp)(const char*);

int
g (const char* d)
{
  printf ("g");
  return 0;
}

f ()
{
  int errcnt=0;

  if (gp != g)
    {
      printf ("f");
      errcnt++;
    }
}
