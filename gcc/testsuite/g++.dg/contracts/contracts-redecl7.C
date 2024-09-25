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
  friend int later(int a, T *t) [[ pre: a > 0 ]] [[ pre: t->pri > 0 ]]
  {
    printf("later: a: %d, t->pri: %d\n", a, t->pri);
    return -a * t->pri;
  }
  friend int both(int a, T *t) [[ pre: a > 0 ]]
  {
    printf("both: a: %d, t->pri: %d\n", a, t->pri);
    return -a * t->pri;
  }


  friend int S::now(int a, T *t);

  friend int hidden(int a, T *t) [[ pre: a > 0 ]] [[ pre: t->pri > 0 ]]
  {
    printf("hidden: a: %d, t->pri: %d\n", a, t->pri);
    return -a * t->pri;
  }
  friend int hidden2(int a, T *t) [[ pre: a > 0 ]] [[ pre: t->pri > 0 ]]
  {
    printf("hidden2: a: %d, t->pri: %d\n", a, t->pri);
    return -a * t->pri;
  }

  int x{1};
  private:
    int pri{-10};
};

int hidden2(int a, T *t) [[ pre: a > 0 ]] [[ pre: t->pri > 0 ]];

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
  later(-21, &t);
  both(-22, &t);
  hidden(-23, &t);
  hidden2(-24, &t);
  return 0;
}

// { dg-skip-if "requires hosted libstdc++ for cstdio" { ! hostedlib } }
// { dg-output "contract violation in function S::now at .*.C:9: .*(\n|\r\n|\r)" }
// { dg-output "S::now: a: -10, t->pri: -10(\n|\r\n|\r)" }
// { dg-output "contract violation in function now at .*.C:15: .*(\n|\r\n|\r)" }
// { dg-output "now: a: -20, t->pri: -10(\n|\r\n|\r)" }
// { dg-output "contract violation in function later at .*.C:22: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function later at .*.C:22: .*(\n|\r\n|\r)" }
// { dg-output "later: a: -21, t->pri: -10(\n|\r\n|\r)" }
// { dg-output "contract violation in function both at .*.C:27: .*(\n|\r\n|\r)" }
// { dg-output "both: a: -22, t->pri: -10(\n|\r\n|\r)" }
// { dg-output "contract violation in function hidden at .*.C:36: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function hidden at .*.C:36: .*(\n|\r\n|\r)" }
// { dg-output "hidden: a: -23, t->pri: -10(\n|\r\n|\r)" }
// { dg-output "contract violation in function hidden2 at .*.C:41: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function hidden2 at .*.C:41: .*(\n|\r\n|\r)" }
// { dg-output "hidden2: a: -24, t->pri: -10(\n|\r\n|\r)" }

