void abort (void);
void exit (int);

void *foo[]={(void *)&("X"[0])};

int
main (void)
{
  if (((char*)foo[0])[0] != 'X')
    abort ();
  exit (0);
}
