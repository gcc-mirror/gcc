// PR c++/86773
// { dg-do compile { target c++17 } }

template <typename ... Param>
auto work(Param && ...param)
{
  return ("asda" ... / param); // { dg-error "expected" }
}

int main()
{
  work(1.0, 2.0, 5, 4.0);
}
