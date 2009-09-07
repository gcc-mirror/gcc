struct S
{
  unsigned int iu;
};

union U
{
  struct S s;
  signed int is;
};

extern signed int bar ();

struct S foo (void)
{
  union U u;

  u.is = bar ();
  return u.s;
}
