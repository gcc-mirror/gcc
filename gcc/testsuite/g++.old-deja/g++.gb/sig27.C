// Special g++ Options: -fhandle-signatures
// GROUPS passed gb sigptr multiple-inheritance
// Test class defined by multiple inheritance as implementation of signature.

extern "C"
{
  int printf (char *, ...);
}

class C
{
public:
  char * f (void) { return "PA"; }
};

class D
{
public:
  char * g (void) { return "SS"; }
};

class E : public C, public D { };

signature S
{
  char * f (void);
  char * g (void);
};

int main (void)
{
  E a;
  S * p = &a;

  printf ("%s%s\n", p->f (), p->g ());

  return 0;
}
