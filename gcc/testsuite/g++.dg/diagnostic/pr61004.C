// { dg-do compile }
// { dg-options "-O2 -Wall" }

struct A{ };
struct B:A{};
void f(A const&);
int main()
{
  B b;
  f(b); // { dg-bogus "strict-aliasing" }
}
