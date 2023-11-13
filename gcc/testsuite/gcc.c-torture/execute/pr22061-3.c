void abort (void);
void exit (int);

void
bar (int N)
{
  int foo (char a[2][++N]) { N += 4; return sizeof (a[0]); }
  if (foo (0) != 2)
    abort ();
  if (foo (0) != 7)
    abort ();
  if (N != 11)
    abort ();
}

int
main()
{
  bar (1);
  exit (0);
}
