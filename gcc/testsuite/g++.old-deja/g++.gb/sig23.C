// Special g++ Options: -fhandle-signatures
// GROUPS passed gb sigptr assignment local-signature
// Test assignment to signature pointer of local signature.

extern "C"
{
  int printf (char *, ...);
}

char * PA (void)
{
  class C
  {
  public:
    char * f (void) { return "PA"; }
  };

  signature S
  {
    char * f (void);
  };

  C a;
  S * p = &a;

  return p->f ();
}

char * SS (void)
{
  class C
  {
  public:
    char * f (void) { return "SS"; }
  };

  signature S
  {
    char * f (void);
  };

  C a;
  S * p = &a;

  return p->f ();
}

int main (void)
{
  printf ("%s%s\n", PA (), SS ());

  return 0;
}
