// Special g++ Options: -fhandle-signatures
// GROUPS passed gb sigptr overloading
// Test overloading of signature member functions.

extern "C"
{
  int printf (char *, ...);
}

class C
{
public:
  char * f (void) { return "PA"; }
  char * f (int)  { return "SS"; }
};

signature S
{
  char * f (void);
  char * f (int);
};

int main (void)
{
  C a;
  S * p;

  p = &a;

  printf ("%s%s\n", p->f (), p->f (0));

  return 0;
}
