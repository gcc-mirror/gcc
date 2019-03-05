/*
TEST_OUTPUT:
---
fail_compilation/ice12397.d(12): Error: undefined identifier `tokenLookup`
---
*/

struct DSplitter
{
    enum Token : int
    {
        max = tokenLookup.length
    }

    immutable string[Token.max] tokenText;
}
