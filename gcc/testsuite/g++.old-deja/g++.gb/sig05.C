// Special g++ Options: -fhandle-signatures
// GROUPS passed gb sigptr initialization
// Test initialization of a local signature pointer.

extern "C"
{
  int printf (char *, ...);
}

class C
{
public:
  char * f (void) { return "PA"; }
  char * g (int)  { return "SS"; }
};

signature S
{
  char * f (void);
  char * g (int);
};

int main (void)
{
  C a;
  S * p = &a;

  printf ("%s%s\n", p->f (), p->g (0));

  return 0;
}
