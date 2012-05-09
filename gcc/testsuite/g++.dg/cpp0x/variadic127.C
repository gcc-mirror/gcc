// PR c++/51148
// { dg-do compile { target c++11 } }

template<typename... Types>
struct S
{};

template<typename... Types>
struct T
{
  friend class S<Types>;     // { dg-error "parameter packs not expanded" }
};

int main()
{}
