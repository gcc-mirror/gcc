// Special g++ Options: -fhandle-signatures
// GROUPS passed gb sigptr argument return-value new
// Test passing a signature pointer to a function and returning it from one.

extern "C"
{
  int printf (char *, ...);
}

class C
{
  char * text;
public:
  C (char * s) { text = s; }
  char * get_msg (void) const { return text; }
};

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
  printf ("%s%s\n", f(new C ("PA")), g(new C ("SS"))->get_msg ());

  return 0;
}
