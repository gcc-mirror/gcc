// https://issues.dlang.org/show_bug.cgi?id=19608
/*
TEST_OUTPUT:
---
fail_compilation/test19608.d(15): Error: cannot pass function `*& f` as a function argument
---
*/
import core.stdc.stdarg;

void f(int) {}
void g(...) {}
void h()
{
    g(&f);  // OK, function address
    g(*&f); // ICE -> Error
}
