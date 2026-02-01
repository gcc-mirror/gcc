/*
TEST_OUTPUT:
---
fail_compilation/pragmamangle2.d(37): Error: `pragma(mangle)` expects string literal argument for mangled name
fail_compilation/pragmamangle2.d(39): Error: `pragma(mangle)` expects string literal argument for mangled name
fail_compilation/pragmamangle2.d(46): Error: `pragma(mangle)` must be attached to a declaration
fail_compilation/pragmamangle2.d(48): Error: `pragma(mangle)` must be attached to a declaration
fail_compilation/pragmamangle2.d(55): Error: `pragma(mangle)` expects string literal argument for mangled name
fail_compilation/pragmamangle2.d(58): Error: `pragma(mangle)` zero-length string not allowed for mangled name
fail_compilation/pragmamangle2.d(61): Error: `pragma(mangle)` expects 1 or 2 arguments
fail_compilation/pragmamangle2.d(64): Error: `pragma(mangle)` cannot apply to a template declaration
fail_compilation/pragmamangle2.d(64):        use `template cannot_apply(Args...) { pragma(mangle, "template") ... }`
fail_compilation/pragmamangle2.d(67): Error: `pragma(mangle)` takes a single argument that must be a string literal
fail_compilation/pragmamangle2.d(70): Error: `pragma(mangle)` takes a single argument that must be a string literal
fail_compilation/pragmamangle2.d(73): Error: `string` expected for pragma mangle argument, not `(T)` of type `T`
fail_compilation/pragmamangle2.d(76): Error: `string` expected for pragma mangle argument, not `(T[T])` of type `T[T]`
fail_compilation/pragmamangle2.d(79): Error: `pragma(mangle)` must be attached to a declaration
fail_compilation/pragmamangle2.d(85): Error: `class` or `struct` type expected for pragma mangle argument, not `"mangle"` of type `string`
fail_compilation/pragmamangle2.d(88): Error: `class` or `struct` type expected for pragma mangle argument, not `F()` of type `T`
fail_compilation/pragmamangle2.d(91): Error: `class` or `struct` type expected for pragma mangle argument, not `F()` of type `T`
fail_compilation/pragmamangle2.d(94): Error: `string` expected for pragma mangle argument, not `(F())` of type `T`
fail_compilation/pragmamangle2.d(97): Error: `string` expected for pragma mangle argument, not `(F())` of type `T`
fail_compilation/pragmamangle2.d(100): Error: `string` expected for pragma mangle argument, not `(V)` of type `T`
fail_compilation/pragmamangle2.d(103): Error: `string` expected for pragma mangle argument, not `(V)` of type `T`
fail_compilation/pragmamangle2.d(106): Error: `string` expected for pragma mangle argument, not `T` of type `T`
fail_compilation/pragmamangle2.d(109): Error: `class` or `struct` type expected for pragma mangle argument, not `int` of type `int`
fail_compilation/pragmamangle2.d(112): Error: `class` or `struct` type expected for pragma mangle argument, not `T[T]` of type `T[T]`
---
*/
void pragma_statement_test()
{
    struct T { }
    T F() { return T(); }
    T V;
    alias A = T[T];

    pragma(mangle);

    pragma(mangle)
    {
        synchronized
        {
        }
    }

    pragma(mangle, "no_declaration");

    pragma(mangle, "no_declaration")
    {
        synchronized
        {
        }
    }

    pragma(mangle)
    void no_string_literal();

    pragma(mangle, "")
    void empty_string_mangle();

    pragma(mangle, "too", "many", "arguments")
    void expects_less_arguments();

    pragma(mangle, "template")
    template cannot_apply(T) { }

    pragma(mangle, "func", "mangle")
    void func_too_many_arguments();

    pragma(mangle, "var", "mangle")
    static int var_too_many_arguments;

    pragma(mangle, T)
    void func_arg1_not_a_string();

    pragma(mangle, A)
    static int var_arg1_not_a_string;

    pragma(mangle, "too_many_declarations")
    {
        nothrow pure void func1();
        @nogc extern(C++) void func2();
    }

    pragma(mangle, "struct", "mangle")
    struct arg2_must_be_symbol { }

    pragma(mangle, "struct", F)
    struct struct_wrong_symbol { }

    pragma(mangle, "class", F)
    class class_wrong_symbol { }

    pragma(mangle, F(), T)
    struct arg1_not_ctfe_string { }

    pragma(mangle, T, F())
    struct arg2_not_ctfe_string { }

    pragma(mangle, V, T)
    struct arg1_not_string { }

    pragma(mangle, T, V)
    struct arg2_not_string { }

    pragma(mangle, T, T)
    struct arg2_expect_string { }

    pragma(mangle, int)
    struct arg1_not_class_or_struct { }

    pragma(mangle, "struct", A)
    struct arg2_not_class_or_struct { }
}
