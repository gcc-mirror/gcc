/*
TEST_OUTPUT:
---
fail_compilation/fail194.d(18): Error: function & foo is overloaded
---
*/

import core.vararg;

void bar(int i, ...) { }

void foo() { }
void foo(int) { }

void main()
{
    //bar(1, cast(void function())&foo);
    bar(1, &foo);
}
