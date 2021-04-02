// PR c++/99063
// { dg-do compile { target c++11 } }

template <typename... T>
void f (T... n)
{
  do
    {
    }
  while (--n); // { dg-error "parameter packs not expanded with '...'" }
}

void g ()
{
  f(3);
}
