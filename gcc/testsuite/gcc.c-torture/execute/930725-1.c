void abort (void);
void exit (int);

int v;

char *
g ()
{
  return "";
}

char *
f ()
{
  return (v == 0 ? g () : "abc");
}

int
main (void)
{
  v = 1;
  if (!__builtin_strcmp (f (), "abc"))
    exit (0);
  abort();
}
