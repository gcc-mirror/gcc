// Special g++ Options: -fhandle-signatures
// GROUPS passed gb sigptr multiple-inheritance
// Test calling overwritten virtual functions through signature pointer.

extern "C"
{
  int printf (char *, ...);
}

class C
{
public:
  char * text1;
  C () { text1 = "PA"; }
  virtual char * f (void) = 0;
};

class D
{
public:
  char * text2;
  D () { text2 = "SS"; }
  virtual char * g (void) = 0;
};

class E : public C, public D
{
public:
  E () : C (), D () { };
  char * f (void) { return text1; }
  char * g (void) { return text2; }
};

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
