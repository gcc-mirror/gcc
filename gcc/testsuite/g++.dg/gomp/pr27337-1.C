// PR middle-end/27337
// { dg-do compile }

struct S
{
  S ();
  ~S ();
  double &operator* () const;
};

S
foo ()
{
  int i;
  S ret;

#pragma omp parallel for
  for (i = 0; i < 2; i++)
    *ret += i;

  return ret;
}
