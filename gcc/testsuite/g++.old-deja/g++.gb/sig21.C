// Special g++ Options: -fhandle-signatures
// GROUPS passed gb sigptr assignment virtual
// Test assignment of objects of different types to same signature pointer.

extern "C"
{
  int printf (char *, ...);
}

class C
{
public:
  virtual char * f (void) = 0;
};

class D : public C
{
public:
  char * f (void) { return "P"; }
};

class E
{
public:
  char * f (void) { return "AS"; }
};

class F : public C
{
public:
  char * f (void) { return "S"; }
};

signature S
{
  char * f (void);
};

int main (void)
{
  E a;
  C * p = new D;
  S * q;

  q = p;
  printf ("%s", q->f ());

  q = &a;
  printf ("%s", q->f ());

  p = new F;
  q = p;
  printf ("%s\n", q->f ());

  return 0;
}
