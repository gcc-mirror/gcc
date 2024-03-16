void abort (void);
void exit (int);

int foo (void)
{
  int i;
#line 1 "20010206-1.c"
  if (0) i = 1; else i
#line 1 "20010206-1.c"
    = 26;
  return i;
}

int main ()
{
  if (foo () != 26)
    abort ();
  exit (0);
}
