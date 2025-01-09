/*
 * TEST_OUTPUT:
---
fail_compilation/test15672.d(17): Error: cast from `void[]` to `byte[]` is not allowed in a `@safe` function
fail_compilation/test15672.d(17):        `void` data may contain pointers and target element type is mutable
fail_compilation/test15672.d(27): Error: cast from `void*` to `byte*` is not allowed in a `@safe` function
fail_compilation/test15672.d(27):        `void` data may contain pointers and target element type is mutable
---
*/
// https://issues.dlang.org/show_bug.cgi?id=15672

alias byte T;
alias const(byte) CT;

@safe T[] test1(void[] a)
{
    return cast(T[])a;
}

@safe CT[] test2(void[] a)
{
    return cast(CT[])a;
}

@safe T* test3(void* a)
{
    return cast(T*)a;
}

@safe CT* test4(void* a)
{
    return cast(CT*)a;
}

@safe T[] test5()
{
    return cast(T[])[];
}
