// PR c++/95164
// { dg-do compile { target c++11 } }
// { dg-options "-Wmissing-braces" }

struct H {
  int a;
};

struct X : H { };

struct I {
  int c;
  H b;
};
struct E { I d; };
void foo(E);

template<int N>
void fn ()
{
  int a = 42;
  int &k = a;

  foo({1, {H{k}}}); // { dg-warning "missing braces around initializer for .I." }
  foo({1, {X{k}}}); // { dg-warning "missing braces around initializer for .I." }

  foo({{1, {k}}});
  foo({{1, {N}}});

  foo({{1, H{k}}});
  foo({{1, H{N}}});
  foo({{1, X{k}}});
  foo({{1, X{N}}});

  foo({{1, {H{k}}}});
  foo({{1, {H{N}}}});
  foo({{1, {X{k}}}});
  foo({{1, {X{N}}}});
}
