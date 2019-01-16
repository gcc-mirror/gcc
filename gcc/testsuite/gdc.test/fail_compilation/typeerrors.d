/*
PERMUTE_ARGS:
TEST_OUTPUT:
---
fail_compilation/typeerrors.d(36): Error: tuple index 4 exceeds 4
fail_compilation/typeerrors.d(38): Error: variable x cannot be read at compile time
fail_compilation/typeerrors.d(39): Error: can't have array of void()
fail_compilation/typeerrors.d(40): Error: cannot have array of scope typeerrors.C
fail_compilation/typeerrors.d(41): Error: cannot have array of scope typeerrors.C
fail_compilation/typeerrors.d(44): Error: int[5] is not an expression
fail_compilation/typeerrors.d(46): Error: x is used as a type
fail_compilation/typeerrors.d(47): Error: can't have associative array key of void()
fail_compilation/typeerrors.d(48): Error: can't have associative array key of void
fail_compilation/typeerrors.d(49): Error: cannot have array of scope typeerrors.C
fail_compilation/typeerrors.d(50): Error: can't have associative array of void
fail_compilation/typeerrors.d(51): Error: can't have associative array of void()
fail_compilation/typeerrors.d(53): Error: cannot have parameter of type void
fail_compilation/typeerrors.d(55): Error: slice [1..5] is out of range of [0..4]
fail_compilation/typeerrors.d(56): Error: slice [2..1] is out of range of [0..4]
---
*/





template tuple(T...) { alias T tuple; }

void bar();

scope class C { }

void foo()
{
    alias T = tuple!(1,2,int,7);
    T[4] a;
    int x;
    T[x] b;
    typeof(bar)[5] c;
    C[6] d;
    C[] e;

    alias int[5] AI;
    auto f = AI.ptr;

    int[x*] g;
    int[typeof(bar)] h;
    int[void] i;
    C[int] j;
    void[int] k;
    typeof(bar)[int] l;

    void abc(void) { }

    alias T2 = T[1 .. 5];
    alias T3 = T[2 .. 1];
}

