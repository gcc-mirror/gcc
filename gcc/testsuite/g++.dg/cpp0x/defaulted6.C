// PR c++/37906
// { dg-do compile { target c++11 } }

struct b
{
  b() = default;
  b(const b&) = delete;
};

void test01()
{
  static_assert(__has_trivial_constructor(b), "default ctor not trivial");
}
