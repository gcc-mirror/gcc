// { dg-do compile }

void foo(void);
int foo(int);
template <typename T> void bar(T f);

void baz() {
  bar(foo); // { dg-error "<unresolved overloaded function type>" }
}
