// PR middle-end/33333
// { dg-do compile }

struct A
{
  int n;
  void foo ();
};

void
A::foo ()
{
  try
  {
    #pragma omp parallel for
      for (int i = 0; i < n; ++i)
	;
  } catch (...) {}
}
