// Special g++ Options: -fhandle-signatures
// GROUPS passed gb sigptr cast
// Test casting a signature pointer to a class pointer.

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
  S * p;
  C * q;

  p = &a;
  q = (C *) p;

  printf ("%s%s\n", q->f (), q->g (0));

  return 0;
}
