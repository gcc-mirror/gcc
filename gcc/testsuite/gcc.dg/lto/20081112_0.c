extern void exit (int);
extern void abort (void);

extern void f ();
extern int g ();

int
main ()
{
  f ();
  if (g () != 42)
    abort ();
  exit (0);
}
