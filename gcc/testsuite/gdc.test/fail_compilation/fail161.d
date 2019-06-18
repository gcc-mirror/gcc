/*
TEST_OUTPUT:
---
fail_compilation/fail161.d(15): Error: template instance `MetaString!"2 == 1"` does not match template declaration `MetaString(String)`
---
*/

template MetaString(String)
{
    alias String Value;
}

void main()
{
    alias MetaString!("2 == 1") S;
    assert(mixin(S.Value));
}
