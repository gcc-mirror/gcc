// Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net>
// Origin: PRs 7721 and 7803
// { dg-do compile }

namespace N
{
    template<typename> 
    struct X { };
}

N::X X;                           // { dg-error "" "" }

int main()
{
    return sizeof(X);             // { dg-error "" "" }
}
