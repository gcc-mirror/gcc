/*
TEST_OUTPUT:
---
fail_compilation/fail297.d(30): Error: incompatible types for ((Bar()) + (baz())): 'Bar' and 'const(Bar)'
---
*/

// Issue 1969 - ICE(cod1.c) using undefined operator with one const operand

// 1969  ICE or wrong-code. D2 only. Internal error: backend\cod1.c 1673
/* Root cause: BinExp::typeCombine() is checking for an _exact_ match, but
typeMerge() will return success.

PATCH: cast.c BinExp::typeCombine().
Compare the immutable versions of the types, instead of the types themselves.

    if (op == TOKmin || op == TOKadd)
    {
        if (t1->ito == t2->ito && (t1->ty == Tstruct || t1->ty == Tclass))
            goto Lerror;
    }
*/

struct Bar {}

const(Bar) baz() { return Bar(); }

void foo()
{
    Bar result = Bar() + baz();
}
