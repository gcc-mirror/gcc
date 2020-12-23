// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=98427
// { dg-do compile }
// { dg-options "-O2 -fno-inline" }

@trusted memoizeExpr()
{
    struct CodepointSet
    {
        struct CowArray
        {
            uint *ptr;
        }

        const CodepointSet binary(U)(U rhs)
        {
            return rhs;
        }

        CowArray array;
    }

    CodepointSet().binary(CodepointSet());
}
