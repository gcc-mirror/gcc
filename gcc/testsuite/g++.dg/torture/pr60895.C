// { dg-do compile }

struct C
{
  double elems[3];
};

C
foo ()
{
  C a;
  double *f = a.elems;
  int b;
  for (; b;)
    {
      *f = 0;
      ++f;
    }
  return a;
}

struct J
{
  C c;
  __attribute__((always_inline)) J () : c (foo ()) {}
};

void
bar ()
{
  J ();
}
