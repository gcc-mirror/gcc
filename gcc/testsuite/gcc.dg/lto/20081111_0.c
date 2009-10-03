extern int mumble;
extern void abort (void);
extern void exit (int);

int
main ()
{
  if (++mumble != 42)
    abort ();
  exit (0);
}
