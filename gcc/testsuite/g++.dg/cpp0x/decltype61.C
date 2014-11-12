// DR 1510, PR c++/60420
// { dg-do compile { target c++11 } }

struct MyIter
{
  int& operator*();
};

void foo(MyIter begin)
{
  auto x = [](const decltype(*begin)) { };
}

template<typename Iterator>
void bar(Iterator begin)
{
  auto x = [](const decltype(*begin)) { };
}

template void bar<MyIter>(MyIter);
