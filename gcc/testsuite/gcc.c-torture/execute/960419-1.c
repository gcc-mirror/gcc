void abort (void);
void exit (int);

static int i;

void
check(x)
     int x;
{
  if (!x)
    abort();
}

int
main(void)
{
  int *p = &i;

  check(p != (void *)0);
  exit (0);
}
