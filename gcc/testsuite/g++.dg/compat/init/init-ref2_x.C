extern "C" void abort (void);
extern void g (void);
extern void h (void);

int r;
int c;
int f ()
{
  // Test that we only initialize i once.
  if (++c > 1)
    ++r;
  return 42;
}

void
init_ref2_x (void)
{
  g ();
  h ();
  if (r != 0)
    abort ();
}
