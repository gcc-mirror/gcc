// { dg-options "--std=c++11" }
// { dg-do link }

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
