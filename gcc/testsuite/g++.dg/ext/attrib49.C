// PR c++/60765
// { dg-options "-Wall -Wunused-parameter" }

struct foo
{
} x;

void (foo::*g) (int *) __attribute__ ((nonnull (2)));

void
fun1 (void (foo::*f) (int *) __attribute__ ((nonnull (2))))
{
    (x.*f) ((int *) 0); // { dg-warning "null argument" }
}

void
fun2 (void (foo::*f) () __attribute__ ((nonnull, unused))) // { dg-bogus "unused" }
{
    (x.*g) ((int *) 0); // { dg-warning "null argument" }
}
