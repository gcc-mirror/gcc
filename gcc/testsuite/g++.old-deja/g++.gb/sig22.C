// Special g++ Options: -fhandle-signatures
// GROUPS passed gb sigptr assignment initialization
// Test assignment to/init of sigptr that requires copying sigtable slots.

extern "C"
{
  int printf (char *, ...);
}

class C
{
public:
  int f (void) { return 1; }
  int g (int)  { return 2; }
  int h (void) { return 666; }
};

signature S
{
  int f (void);
  int g (int);
  int h (void);
};

signature T
{
  int f (void);
  int h (void);
};

signature U
{
  int h (void);
  int g (int);
};

C a;
S * p  = &a;
T * q1 = p;
U * r1 = p;

int main (void)
{
  C a;
  S * p  = &a;
  T * q2 = p;
  U * r2 = p;;
  T * q3;
  U * r3;

  q3 = p;
  r3 = p;

  if (q1->f () + q2->f () + q3->f () == 3
      && r1->g (0) + r2->g (0) + r3->g (0) == 6)
    printf ("PASS\n");

  return 0;
}
