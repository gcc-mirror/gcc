// PR c++/66387
// { dg-do compile { target c++11 } }

template <typename T>
void
bar (T x)
{
  x ();
}

void
foo ()
{
  constexpr int a[1] = { 1 };
  bar ([&]{ return a[0]; });
}
