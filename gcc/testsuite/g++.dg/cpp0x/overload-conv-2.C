// { dg-do link { target c++11 } }

struct T {};
struct S
{
  S(T const &) {}
};

void f(const S&);
void f(S&&) {}

int main()
{
  T t;
  f(t);
}
