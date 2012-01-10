// { dg-do compile }
// { dg-require-effective-target lto }
// { dg-options "-flto" }

void foo();

static void bar() __attribute__((weakref("foo")));

struct A
{
    A();
};

int i;

template <typename T, int&> struct B : T {};

B<A, i> b;
