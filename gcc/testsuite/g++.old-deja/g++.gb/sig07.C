// Special g++ Options: -fhandle-signatures
// GROUPS passed gb sigptr default-argument
// Test calling a signature member function with default argument.

extern "C"
{
  int printf (char *, ...);
}

class C
{
public:
  char * f (char * text) { return text; }
};

signature S
{
  char * f (char * text = "PA");
};

C a;
S * p = &a;

int main (void)
{
  printf ("%s%s\n", p->f (), p->f ("SS"));

  return 0;
}
