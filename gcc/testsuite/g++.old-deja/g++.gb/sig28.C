// Special g++ Options: -fhandle-signatures
// GROUPS passed gb sigptr multiple-inheritance
// Test correct adjustment of `this' pointer in case of multiple inheritance.

extern "C"
{
  int printf (char *, ...);
}

class C
{
  char * text;
public:
  C () { text = "PA"; }
  char * f (void) { return text; }
};

class D
{
  char * text;
public:
  D () { text = "SS"; }
  char * g (void) { return text; }
};

class E : public C, public D
{
public:
  E () : C (), D () { }
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
