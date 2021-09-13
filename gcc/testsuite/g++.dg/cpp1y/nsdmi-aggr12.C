// PR c++/90926
// { dg-do run { target c++14 } }

#include <cassert>

struct A
{
  char str[4] = "foo";
  char str_array[2][4] = {"bar", "baz"};
};

struct B
{
  char16_t str[10];
};

int called = 0;
void f(A) { called = 1;};
void f(B) { called = 2;};

int
main ()
{
  A a;
  a.str[0] = 'g';
  a.str_array[0][0] = 'g';
  a = {};

  if (__builtin_strcmp (a.str, "foo") != 0)
    __builtin_abort();
  if (__builtin_strcmp (a.str_array[0], "bar") != 0)
    __builtin_abort();

  f({"foo"}); assert(called == 1);
  f({u"foo"}); assert(called == 2);
}
