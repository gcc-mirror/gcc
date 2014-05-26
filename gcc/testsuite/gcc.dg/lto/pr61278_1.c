extern char foo (char *);

char d;

int
main ()
{
  foo (&d);
  return 0;
}
