// PR c++/89158
// { dg-do compile { target c++11 } }

struct T { T(const int&); };
void Func(T);

void test()
{
  constexpr int Val = 42;
  [Val]() { Func(Val); };
}
