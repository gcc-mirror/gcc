/*
 * TEST_OUTPUT:
---
fail_compilation/test15672.d(15): Error: cast from `void[]` to `byte[]` not allowed in safe code
fail_compilation/test15672.d(25): Error: cast from `void*` to `byte*` not allowed in safe code
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


