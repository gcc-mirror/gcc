/*
TEST_OUTPUT:
---
4
false
false
---
*/

// https://issues.dlang.org/show_bug.cgi?id=20339

struct S
{
    pragma(msg, cast(int)S.sizeof);

    this(this){}
    ~this(){}

    int f;
}

static assert(__traits(isPOD, S) == false);


pragma(msg, __traits(isPOD, T));

struct T
{
    this(this){}
    ~this(){}
}

static assert(__traits(isPOD, T) == false);


struct U
{
    pragma(msg, __traits(isPOD, U));

    this(this){}
    ~this(){}
}

static assert(__traits(isPOD, U) == false);
