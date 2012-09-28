// Origin: PR c++/29028
// { dg-options "-Wunused" }
// { dg-do compile }

namespace N
{
    int i; // { dg-warning "unused variable" }
}

void
f ()
{
    using N::i;
}
