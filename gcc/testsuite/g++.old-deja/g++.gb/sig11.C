// Special g++ Options: -fhandle-signatures
// GROUPS passed gb sigptr array
// Test a signature pointer array.

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

int main (void)
{
  S * p[2] = { new C, new D };

  printf ("%s%s\n", p[0]->f (), p[1]->f ());

  return 0;
}
