// Special g++ Options: -fhandle-signatures
// GROUPS passed gb sigptr assignment initialization new
// Test assignment to/initialization of signature pointer with run-time value.

extern "C"
{
  int printf (char *, ...);
}

class C
{
public:
  int f (void) { return 1; }
};

signature S
{
  int f (void);
};

S * p1 = new C;

int main (void)
{
  S * p2 = new C;
  S * p3;

  p3 = new C;

  if (p1->f () + p2->f () + p3->f () == 3)
    printf ("PASS\n");

  return 0;
}
