// Special g++ Options: -fhandle-signatures
// GROUPS passed gb sigptr default-implementation
// Test calling default implementation through signature pointer.

extern "C"
{
  int printf (char *, ...);
}

class C { };

class D
{
public:
  char * f (void) { return "SS"; }
};

signature S
{
  char * f (void) { return "PA"; }
  char * g (void) { return f (); }
};

int main (void)
{
  C a;
  D b;
  S * p = &a;
  S * q = &b;

  printf ("%s%s\n", p->g (), q->g ());

  return 0;
}
