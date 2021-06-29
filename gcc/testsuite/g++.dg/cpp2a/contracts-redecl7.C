// test that contracts can be added during (defining) friend declarations
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on" }
#include <cstdio>
struct T;

struct S
{
  int now(int a, T *t) [[ pre: a > 0 ]] [[ pre: x < 0 ]];


  int x{-1};
};

int now(int a, T *t) [[ pre: a > 0 ]];
int later(int a, T *t);
int both(int a, T *t) [[ pre: a > 0 ]];

struct T
{
  friend int now(int a, T *t);
  friend int both(int a, T *t) [[ pre: a > 0 ]]
  {
    printf("both: a: %d, t->pri: %d\n", a, t->pri);
    return -a * t->pri;
  }

  friend int S::now(int a, T *t);

  int x{1};
  private:
    int pri{-10};
};

int S::now(int a, T *t)
{
  printf("S::now: a: %d, t->pri: %d\n", a, t->pri);
  return -a * t->pri;
}

int now(int a, T *t)
{
  printf("now: a: %d, t->pri: %d\n", a, t->pri);
  return -a * t->pri;
}

int main(int, char**)
{
  T t;
  S s;
  s.now(-10, &t);

  now(-20, &t);
  both(-22, &t);
  return 0;
}

// { dg-output "default std::handle_contract_violation called: .*.C 9 S::now .*(\n|\r\n|\r)*" }
// { dg-output "S::now: a: -10, t->pri: -10(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 15 now .*(\n|\r\n|\r)*" }
// { dg-output "now: a: -20, t->pri: -10(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 22 both .*(\n|\r\n|\r)*" }
// { dg-output "both: a: -22, t->pri: -10(\n|\r\n|\r)*" }

