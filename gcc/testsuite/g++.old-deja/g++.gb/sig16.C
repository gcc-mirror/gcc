// Special g++ Options: -fhandle-signatures
// GROUPS passed gb sigptr cast
// Test casting a class pointer to an anonymous signature pointer.

extern "C"
{
  int printf (char *, ...);
}

class C
{
public:
  char * f (void) { return "PA"; }
  char * g (int)  { return "S"; }
  char * h (void) { return "FAIL"; }
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

C a;
signature { char * g (int); } * r1 =
  (signature { char * h (void); char * g (int); } *) &a;

int main (void)
{
  C a;
  S * p = &a;
  T * q = p;
  signature { char * g (int); } * r2 =
    (signature { char * h (void); char * g (int); } *) &a;

  printf ("%s%s%s\n", q->f (), r1->g (0), r2->g (0));

  return 0;
}
