// Special g++ Options: -fhandle-signatures
// GROUPS passed gb sigptr assignment initialization
// Test assignment of/initialization with same type signature pointer.

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

C a;
S * p1 = &a;
S * q1 = p1;

int main (void)
{
  C a;
  S * p2 = &a;
  S * q2 = p2;
  S * q3;

  q3 = p2;

  if (q1->f () + q2->f () + q3->f () == 3)
    printf ("PASS\n");

  return 0;
}
