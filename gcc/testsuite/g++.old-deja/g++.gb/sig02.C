// Special g++ Options: -fhandle-signatures
// GROUPS passed gb sigptr assignment
// Test assigning objects of different classes to local signature pointer.

extern "C"
{
  int printf (char *, ...);
}

class C
{
public:
  char * f (void) { return "PA"; }
};

class D
{
public:
  char * f (void) { return "SS"; }
};

signature S
{
  char * f (void);
};

int main (void)
{
  C a;
  D b;
  S * p;
  int i;

  for (i = 0; i < 2; i++)
    {
      if (i == 0)
	p = &a;
      else
	p = &b;
      printf ("%s", p->f ());
    }
  printf ("\n");

  return 0;
}
