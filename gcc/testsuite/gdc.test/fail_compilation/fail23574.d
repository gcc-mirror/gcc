// https://issues.dlang.org/show_bug.cgi?id=23574
/*
TEST_OUTPUT:
---
fail_compilation/fail23574.d(26): Error: function `object._xopEquals` has no `return` statement, but is expected to return a value of type `bool`
Error: undefined identifier `size_t` in module `object`
fail_compilation/fail23574.d(34): Error: template instance `object.S17915!(MyClass)` error instantiating
fail_compilation/fail23574.d(30): Error: function `object.SDL_GetKeyName` has no `return` statement, but is expected to return a value of type `const(char)`
---
*/
module object;

class Object
{
}

bool opEquals(LHS, RHS)(LHS lhs, RHS)
{
    opEquals(cast()lhs);
}

class TypeInfo
{
}

bool _xopEquals()
{
}

const(char)SDL_GetKeyName()
{
    class MyClass
    {
        S17915!MyClass m_member;
    }
}

struct S17915(T)
{
    T owner;
}
