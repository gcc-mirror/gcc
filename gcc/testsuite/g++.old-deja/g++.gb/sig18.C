// Special g++ Options: -fhandle-signatures
// GROUPS passed gb sigptr assignment virtual
// Test assignment of object from virtual class to signature pointer.

extern "C"
{
  int printf (char *, ...);
}

class C
{
public:
  virtual char * f (void) { return "PA"; }
  virtual char * g (int)  { return "SS"; }
};

signature S
{
  char * f (void);
  char * g (int);
};

int main (void)
{
  C a;
  S * p;

  p = &a;

  printf ("%s%s\n", p->f (), p->g (0));

  return 0;
}
