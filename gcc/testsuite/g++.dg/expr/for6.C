// PR c++/86769
// { dg-do run { target c++11 } }

int v;

struct S {
  int s;
  S (int x) : s(x)
  {
    if ((v != 0 || s != 0) && (v != 3 || s != 1))
      __builtin_abort ();
    ++v;
  }
  ~S ()
  {
    if ((v != 2 || s != 0) && (v != 4 || s != 1))
      __builtin_abort ();
    ++v;
  }
  operator bool () const { return true; }
};

void
foo (const S &s)
{
  if (v != 1 || s.s != 0)
    __builtin_abort ();
  ++v;
}

int
main ()
{
  for (int i = 0; S j{i}; foo (j))
    {
      if (++i == 2)
	break;
    }
}
