void abort (void);

int bar (void) {  return -1;  }

unsigned long
foo ()
{ unsigned long retval;
  retval = bar ();
  if (retval == -1)  return 0;
  return 3;  }

main ()
{ if (foo () != 0)  abort ();
  return 0;  }

