// { dg-do compile { target c++11 } }
// { dg-options "" }

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
