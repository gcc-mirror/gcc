// Special g++ Options: -fhandle-signatures
// GROUPS passed gb sigptr assignment sigsigtable
// Test use of multiple signature-signature tables for same signature pair.

extern "C"
{
  int printf (char *, ...);
}

class C
{
public:
  virtual int f (void) { return 1; }
  int g (int)          { return 2; }
};

class D : public C
{
public:
  int f (void) { return 3; }
  int g (int)  { return 4; }
};

signature S
{
  int f (void);
  int g (int);
};

signature T
{
  int g (int);
  int f (void);
};

int main (void)
{
  S * p = new C;
  T * q = p;
  T * r;

  p = new D;
  r = p;

  if (q->f () == 1 && q->g (0) == 2
      && r->f () == 3 && r->g (0) == 4)
    printf ("PASS\n");
  else
    printf ("FAIL\n");

  return 0;
}
