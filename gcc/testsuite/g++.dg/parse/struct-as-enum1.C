// PR c++/163, PR c++/8595
// Origin: <martin@loewis.home.cs.tu-berlin.de>, Mark Leone <mleone@pixar.com>
// { dg-do compile }

namespace N
{
    struct A {};
}

typedef enum N::A B; // { dg-error "enum" }
