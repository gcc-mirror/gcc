// Contributed by Gabriel Dos Reis <gdr@codesourcery.com>
// Distilled from PR C++/3656

namespace N
{
    template<typename> struct X { };
}

struct A : N::X { }; // { dg-error "invalid base-class" "" }
