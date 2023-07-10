/*
TEST_OUTPUT:
---
AliasSeq!(func)
---
*/
// https://issues.dlang.org/show_bug.cgi?id=21282

template I(T...) { alias I = T; }

template Bug(T...) {
        alias Bug = mixin("I!(T[0])");
}
void func() {}
pragma(msg, Bug!func);
