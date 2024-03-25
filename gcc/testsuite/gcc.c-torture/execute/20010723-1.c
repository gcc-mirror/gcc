void abort (void);
void exit (int);

int
test ()
{
  int biv,giv;
  for (biv = 0, giv = 0; giv != 8; biv++)
      giv = biv*8;
  return giv;
}

int
main(void)
{
  if (test () != 8)
    abort ();
  exit (0);
}
