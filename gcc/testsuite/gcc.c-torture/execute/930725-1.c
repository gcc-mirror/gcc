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

main ()
{
  v = 1;
  if (!strcmp (f (), "abc"))
    exit (0);
  abort();
}
