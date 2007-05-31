// { dg-options "--std=c++0x" }

struct S { };

struct T
{
  S s;
};

void f(T const &);

void g()
{
  f((T){S()});
}
