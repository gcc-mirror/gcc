// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

int g;

void
fn0 (int, typename [: ^^:: :] i) // { dg-error "reflection .::. not usable in a splice type|declared" }
{
}

void
fn1 (int, typename [: ^^g :] i) // { dg-error "reflection .g. not usable in a splice type|declared" }
{
}

void
fn2 (int, typename [: ^^fn1 :] i) // { dg-error "reflection .fn1. not usable in a splice type|declared" }
{
}

void
fn3 (int p, typename [: ^^p :] i) // { dg-error "reflection .p. not usable in a splice type|declared" }
{
}

enum Harold { Budd };

void
fn4 (int, typename [: ^^Budd :] i) // { dg-error "reflection .Budd. not usable in a splice type|declared" }
{
}

template<int>
struct S {};

void
fn5 (int, typename [: ^^S :] i) // { dg-error "reflection .S. not usable in a splice type|declared" }
{
}
