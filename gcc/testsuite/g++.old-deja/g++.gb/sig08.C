// Special g++ Options: -fhandle-signatures
// GROUPS passed gb sigptr argument return-value
// Test passing a signature pointer to a function and returning it from one.

extern "C"
{
  int printf (char *, ...);
}

class C
{
public:
  char * text;
  char * get_msg (void) const { return text; }
};

const C a = { "PA" };
const C b = { "SS" };

signature S
{
  char * get_msg (void) const;
};

char * f (const S * p)
{
  return p->get_msg ();
}

const S * g (const C * p)
{
  return p;
}

int main (void)
{
  printf ("%s%s\n", f(&a), g(&b)->get_msg ());

  return 0;
}
