// { dg-options "--std=c++11" }

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
