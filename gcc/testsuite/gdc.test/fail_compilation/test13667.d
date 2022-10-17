/*
TEST_OUTPUT:
---
fail_compilation/test13667.d(112): Error: cannot cast expression `this` of type `const(Array1!int)` to `Array1!int*`
fail_compilation/test13667.d(116): Error: template instance `test13667.Array1!int` error instantiating
fail_compilation/test13667.d(121): Error: cannot cast expression `this` of type `const(Array2!int)` to `B*`
fail_compilation/test13667.d(125): Error: template instance `test13667.Array2!int` error instantiating
fail_compilation/test13667.d(136): Error: cannot cast expression `this` of type `const(Array3!int)` to `C*`
fail_compilation/test13667.d(140): Error: template instance `test13667.Array3!int` error instantiating
fail_compilation/test13667.d(151): Error: cannot cast expression `this` of type `const(Array4!int)` to `D*`
fail_compilation/test13667.d(155): Error: template instance `test13667.Array4!int` error instantiating
fail_compilation/test13667.d(172): Error: cannot cast expression `this` of type `const(Array5!int)` to `F*`
fail_compilation/test13667.d(176): Error: template instance `test13667.Array5!int` error instantiating
---
*/

// https://issues.dlang.org/show_bug.cgi?id=13667

#line 100
// 0, no error but also crashed before
struct Array0(T)
{
    Array0!(T) impConv() const { return cast(typeof(return))this; }
    alias impConv this;
}

alias AI0 = Array0!int;

// 1
struct Array1(T)
{
    Array1!(T) impConv() const { return *cast(typeof(return)*)this; }
    alias impConv this;
}

alias AI1 = Array1!int;

// 2
struct Array2(T)
{
    B impConv() const { return cast(B*)this; }
    alias impConv this;
}

alias AI2 = Array2!int;

struct B
{
    AI2 get() { return AI2(); }
    alias get this;
}

// 3
struct Array3(T)
{
    C impConv() const { return cast(C*)this; }
    alias impConv this;
}

alias AI3 = Array3!int;

struct C
{
    C get() { return C(); }
    alias get this;
}

// 4
struct Array4(T)
{
    D impConv() const { return cast(D*)this; }
    alias impConv this;
}

alias AI4 = Array4!int;

struct D
{
    E get() { return E(); }
    alias get this;
}

struct E
{
    AI4 ai;
    alias ai this;
}

// 5: test enum based on struct, needed to use base type (toBasetype())
struct Array5(T)
{
    F impConv() const { return cast(F*)this; }
    alias impConv this;
}

alias AI5 = Array5!int;

enum F : AI5
{
    f = AI5()
}
