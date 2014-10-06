extern void abort (void);
extern void exit (int);
static int v = 3;

void
f ()
{
  int v = 4;
  {
    extern int v; /* { dg-error "static" } */  
    if (v != 3)
      abort ();
  }
}

int
main ()
{
  f ();
  exit (0);
}
