// Origin: PR c++/29028
// { dg-options "-Wunused" }
// { dg-do compile }

namespace N
{
    int i;
}

void
f ()
{
    using N::i; // { dg-warning "unused using" }
}

void
g ()
{
    using N::i; // { dg-bogus "set but not used" }
    i = 10;
}
