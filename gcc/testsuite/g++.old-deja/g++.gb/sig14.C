// Special g++ Options: -fhandle-signatures
// GROUPS passed gb sigptr assignment initialization
// Test assignment of/initialization with different type signature pointer.

extern "C"
{
  int printf (char *, ...);
}

class C
{
public:
  int f (void) { return 1; }
  int g (int)  { return 2; }
};

signature S
{
  int f (void);
  int g (int);
};

signature T
{
  int f (void);
};

signature U
{
  int g (int);
};

C a;
S * p1 = &a;
T * q1 = p1;
U * r1 = p1;

int main (void)
{
  C a;
  S * p2 = &a;
  T * q2 = p2;
  U * r2 = p2;
  T * q3;
  U * r3;

  q3 = p2;
  r3 = p2;

  if (q1->f () + q2->f () + q3->f () == 3
      && r1->g (0) + r2->g (0) + r3->g (0) == 6)
    printf ("PASS\n");

  return 0;
}
