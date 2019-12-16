// PR c++/10385
// Origin: <douglas@coc.ufrj.br>
// { dg-do compile }

struct A
{
  void foo();
};

A& bar();

void baz()
{
  dynamic_cast<A&>( bar().foo );  // { dg-error "3:cannot 'dynamic_cast'" }
}
