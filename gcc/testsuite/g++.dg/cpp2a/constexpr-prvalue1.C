// PR c++/118986
// { dg-do compile { target c++20 } }
// { dg-options "-O" }

struct c1 {
    constexpr c1(int *ptr) {}
};
struct c2 {
    c1 _M_t;
    constexpr ~c2() {}
};
constexpr inline
c2 f1 ()
{
  return c2(new int);
}

void
f ()
{
  auto l = [p = f1()](){};
  [p = f1()](){};
}
