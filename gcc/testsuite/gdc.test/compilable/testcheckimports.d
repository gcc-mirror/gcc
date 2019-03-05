// REQUIRED_ARGS: -transition=checkimports -de
/*
TEST_OUTPUT:
---
---
*/

// https://issues.dlang.org/show_bug.cgi?id=15825

template anySatisfy15825(T...)
{
    alias anySatisfy15825 = T[$ - 1];
}

alias T15825 = anySatisfy15825!(int);

// https://issues.dlang.org/show_bug.cgi?id=15857

template Mix15857(T)
{
    void foo15857(T) {}
}
mixin Mix15857!int;
mixin Mix15857!string;

// will find an overloadset on 2nd lookup w/ SearchImportsOnly set
import imports.test15857a;

void test15857()
{
    foo15857(1);
    bar15857(1);
}
