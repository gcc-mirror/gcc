struct x { int a, b, c; };

extern struct x a ();
extern void b (struct x);

void
foo ()
{
  a ();
  b (a ());
}
