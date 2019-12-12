// { dg-do compile }
// { dg-options "-Wduplicated-cond" }

template <typename>
class a
{
  typedef a b;
  template <typename> void c();
};
template <typename d> template <typename>
void a<d>::c()
{
  int f;
  b g;
  if (g == 0)
    ;
  else if (f)
    {
    }
}
