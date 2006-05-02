// PR middle-end/27337
// { dg-do compile }

struct S
{
  S ();
  ~S ();
  int i;
};

S
foo ()
{
  int i;
  S ret;

#pragma omp parallel for firstprivate (ret) lastprivate (ret)
  for (i = 0; i < 2; i++)
    ret.i += i;

  return ret;
}
