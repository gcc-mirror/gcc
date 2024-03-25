/* A function pointer compared with a void pointer should not be canonicalized.
   See PR middle-end/17564.  */
void abort (void);
void exit (int);

void *f (void) __attribute__ ((__noinline__));
void *
f (void)
{
  return f;
}
int
main (void)
{
  if (f () != f)
    abort ();
  exit (0);
}
