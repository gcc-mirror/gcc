// Special g++ Options: -fhandle-signatures
// GROUPS passed gb sigptr structure
// Test a signature pointer structure.

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
  char * f (void) { return "SS"; }
};

signature S
{
  char * f (void);
};

struct SP
{
  S * p;
  S * q;
};

int main (void)
{
  SP o = { new C, new D };

  printf ("%s%s\n", o.p->f (), o.q->f ());

  return 0;
}
