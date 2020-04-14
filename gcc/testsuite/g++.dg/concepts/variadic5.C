// PR c++/93207
// { dg-do compile { target concepts } }

template <typename... Args>
concept C = true;

struct S
{
    template <typename... Args>
    void f()
    requires C<Args...>;
};

template <typename... Args>
void S::f()
requires C<Args...>
{
}

void foo()
{
  S s;
  s.f<>();
  s.f<int>();
  s.f<int, char>();
}
