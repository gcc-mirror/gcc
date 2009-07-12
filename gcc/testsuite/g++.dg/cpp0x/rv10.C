// { dg-options "-std=gnu++0x" }

struct A
{
  A() = default;
  A(const A&) = delete;
};

A&& f();
void h(A&&);
void g()
{
  A&& arr = f();
  h(f());
}
