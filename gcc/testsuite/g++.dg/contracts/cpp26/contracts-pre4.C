// test that contracts on overriding functions are found correctly
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on -fcontracts-nonattr" }

struct Base
{
  virtual int f(int a) pre ( a > 5 );
};

int Base::f(int a)
{
  return a + 10;
}

// inherits original
struct Child0 : Base
{
};

// defined out of line, explicit override
struct Child1 : Base
{
  virtual int f(int a) override pre ( a > 14 ) ;
};

int Child1::f(int a)
{
  return a + 20;
}

// defined out of line
struct Child2 : Base
{
  int f(int a);
};

int Child2::f(int a)
{
  return a + 30;
}

// defined inline, explicitly override
struct Child3 : Base
{
  virtual int f(int a) override pre ( a > 0 ) 
  {
    return a + 40;
  }
};

// defined inline
struct Child4 : Base
{
  int f(int a)
  {
    return a + 50;
  }
};

#include <cstdio>
int main(int, char**)
{
  Base b;
  Child0 c0;
  Child1 c1;
  Child2 c2;
  Child3 c3;
  Child4 c4;

  printf("Base: %d\n", b.f(3));     // a > 5
  printf("Child0: %d\n", c0.f(3));  // a > 5
  printf("Child1: %d\n", c1.f(7));  // a > 14
  printf("Child2: %d\n", c2.f(1));  // no contracts
  printf("Child3: %d\n", c3.f(0));  // a > 0
  printf("Child3: %d\n", c3.f(1));  // a > 0
  printf("Child4: %d\n", c4.f(0));  // no contracts

  return 0;
}

// { dg-output "contract violation in function Base::f at .*.C:7: a > 5.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function Base::f at .*.C:7: a > 5.*(\n|\r\n|\r)" }
// { dg-output "Base: 13(\n|\r\n|\r)" }
// { dg-output "contract violation in function Base::f at .*.C:7: a > 5.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function Base::f at .*.C:7: a > 5.*(\n|\r\n|\r)" }
// { dg-output "Child0: 13(\n|\r\n|\r)" }
// { dg-output "contract violation in function Child1::f at .*.C:23: a > 14.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function Child1::f at .*.C:23: a > 14.*(\n|\r\n|\r)" }
// { dg-output "Child1: 27(\n|\r\n|\r)" }
// { dg-output "Child2: 31(\n|\r\n|\r)" }
// { dg-output "contract violation in function Child3::f at .*.C:45: a > 0.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function Child3::f at .*.C:45: a > 0.*(\n|\r\n|\r)" }
// { dg-output "Child3: 40(\n|\r\n|\r)" }
// { dg-output "Child3: 41(\n|\r\n|\r)" }
// { dg-output "Child4: 50(\n|\r\n|\r)" }

