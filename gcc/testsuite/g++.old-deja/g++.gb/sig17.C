// Special g++ Options: -fhandle-signatures
// GROUPS passed gb sigptr cast
// Test casting a class pointer through a chain of signature pointers.

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

class D : public C
{
public:
  char * h (void) { return "FAIL"; }
};

class E : public C
{
public:
  int foo (int) { return 0; }
};

signature S
{
  char * f (void);
  char * g (int);
};

signature T
{
  char * f (void);
};

int main (void)
{
  D a;
  S * p;
  T * q;
  signature { char * g (int); } * r;

  p = &a;
  q = (T*) (signature { char * f (void); int foo (int); } *) (E*) (S*) (C*) p;
  r = (signature { char * h (void); char * g (int); } *) &a;

  printf ("%s%s\n", q->f (), r->g (0));

  return 0;
}
