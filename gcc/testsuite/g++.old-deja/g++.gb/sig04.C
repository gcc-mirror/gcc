// Special g++ Options: -fhandle-signatures
// GROUPS passed gb sigptr this
// Test passing of 'this' pointer through signature pointer.

extern "C"
{
  int printf (char *, ...);
}

class C;
int inc_mod_4 (C *);

class C
{
public:
  char * text[4];
  int    i;
  int    get_i (void) { return i; }
  char * msg   (void) { i = inc_mod_4 (this);  return text[i]; }
};

signature S
{
  char * msg (void);
};

int inc_mod_4 (C * p)
{
  return (p->get_i () + 1) % 4;
}

int main (void)
{
  C o = { "P", "A", "S", "S", -1 };
  S * p;
  int i;

  p = &o;

  for (i = 0; i < 4; i++)
    printf ("%s", p->msg ());

  printf ("\n");

  return 0;
}
