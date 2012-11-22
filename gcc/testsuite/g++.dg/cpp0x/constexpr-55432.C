// PR c++/55432
// { dg-do compile { target c++11 } }

struct tag_t{} tag{};

constexpr tag_t const& pass(tag_t & t)
{
  return t;
}

struct S
{
  constexpr S(tag_t)  {};
};

struct T
{
  S mem;
  T( tag_t & args ) : mem(pass(args)) {}
};

T t(tag);
