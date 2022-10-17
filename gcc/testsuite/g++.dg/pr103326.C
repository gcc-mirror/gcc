// { dg-do compile }
// { dg-require-effective-target c++11 }

using x86_64_v16qi [[gnu::__vector_size__ (16)]] = char;

template<typename T>
void foo()
{
  constexpr x86_64_v16qi zero{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
}

void foo2()
{
  foo<int>();
}
