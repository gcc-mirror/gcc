// Compile with -S, there should be no references to
// LTRAMP in the output.

extern "C" int printf (const char *, ...);

void
sub2 (void (*func) ())
{
  (*func) ();
}

int
main(void)
{
  extern void sub (void);

  sub2 (sub);
}

void
sub (void)
{
  printf ("hello world\n");
}
