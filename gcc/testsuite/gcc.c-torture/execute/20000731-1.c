void abort (void);
void exit (int);

double
foo (void)
{
  return 0.0;
}

void
do_sibcall (void)
{
  (void) foo ();
}

int
main (void)
{
   double x;

   for (x = 0; x < 20; x++)
      do_sibcall ();
   if (!(x >= 10))
      abort ();
   exit (0);
}

