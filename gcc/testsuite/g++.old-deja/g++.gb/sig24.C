// Special g++ Options: -fhandle-signatures
// GROUPS passed gb sigptr opaque-type
// Test calling member functions taking/returning opaque type through sigptr.

extern "C"
{
  int printf (char *, ...);
}

signature S
{
  typedef t;
  char * f (t);
  t g (void);
  t h (void);
};

class C
{
public:
  typedef char * t;
  char * f (t text) { return text; }
  t g (void) { return "PA"; }
  t h (void) { return "SS"; }
};

int main (void)
{
  C a;
  S *  p = &a;
  S::t w = p->h ();

  printf ("%s%s\n", p->f (p->g ()), p->f (w));

  return 0;
}
