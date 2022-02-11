/**
TEST_OUTPUT:
---
fail_compilation/already_defined.d(4): Error: declaration `already_defined.func1.a` is already defined
fail_compilation/already_defined.d(3):        `variable` `a` is defined here
fail_compilation/already_defined.d(10): Error: declaration `already_defined.func2.core` is already defined
fail_compilation/already_defined.d(9):        `import` `core` is defined here
fail_compilation/already_defined.d(28): Error: declaration `Ident(T)` is already defined
fail_compilation/already_defined.d(27):        `template` `Ident(T)` is defined here
fail_compilation/already_defined.d(36): Error: declaration `Tstring` is already defined
fail_compilation/already_defined.d(35):        `alias` `Tstring` is defined here
fail_compilation/already_defined.d(42): Error: declaration `T` is already defined
fail_compilation/already_defined.d(41):        `alias` `T` is defined here
fail_compilation/already_defined.d(48): Error: declaration `core` is already defined
fail_compilation/already_defined.d(47):        `import` `core` is defined here
fail_compilation/already_defined.d(54): Error: declaration `core` is already defined
fail_compilation/already_defined.d(53):        `import` `core` is defined here
---
*/

#line 1
void func1 ()
{
    int a;
    bool a;
}

void func2 ()
{
    import core.stdc.stdio;
    string core;
}

void func3 ()
{
    {
        import core.stdc.stdio;
    }

    {
        // No conflict
        string core;
    }
}

void func4 ()
{
    template Ident (T) { alias Ident = T; }
    template Ident (T) { alias Ident = T; }
}

void func5 ()
{
    template Ident (T) { alias Ident = T; }

    alias Tstring = Ident!string;
    alias Tstring = Ident!string;
}

void func6 ()
{
    static if (is(int T == int)) {}
    static if (is(int T == int)) {}
}

void func7 ()
{
    import core.stdc.stdio;
    static if (is(int core == int)) {}
}

void func8 ()
{
    import core.stdc.stdio;
    static if (is(string : core[], core)) {}
}
