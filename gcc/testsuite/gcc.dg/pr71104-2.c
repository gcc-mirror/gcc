/* { dg-do compile } */

struct Foo { char c[1024]; };
void foo(void);
struct Foo baz(void) __attribute__((returns_twice));
struct Foo *p;

void bar(void)
{
  foo();
  *p = baz();
}
