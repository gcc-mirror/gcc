// Special g++ Options: -fhandle-signatures
// GROUPS passed gb sigptr multiple-inheritance
// Test calling virtual function from MI class through signature pointer.

extern "C"
{
  int printf (char *, ...);
}

class C
{
  char * text;
public:
  C () { text = "PA"; }
  virtual char * f (void) { return text; }
};

class D
{
  char * text;
public:
  D () { text = "SS"; }
  virtual char * g (void) { return text; }
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
