// PR c++/92438
// { dg-do compile }

typedef struct S { int x; } T;
T (foo) (T x);
T __attribute__((unused)) bar (T x);
struct S (__attribute__((unused)) baz) (T x);
T (__attribute__((unused)) qux) (T x);

struct U
{
  U (__attribute__((unused)) int);
  U (__attribute__((unused)) corge) (int);
};

void
test ()
{
  T a, b;
  a = foo (b);
  b = bar (a);
  a = baz (b);
  b = qux (a);
  U u (5);
  U v = u.corge (3);
}
