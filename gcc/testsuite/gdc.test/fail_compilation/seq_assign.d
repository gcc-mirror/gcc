/*
TEST_OUTPUT:
---
fail_compilation/seq_assign.d(28): Error: cannot assign `int` to expression sequence `(int)`
fail_compilation/seq_assign.d(29): Error: cannot implicitly convert expression `s` of type `string` to `int`
fail_compilation/seq_assign.d(30): Error: cannot modify constant `2`
fail_compilation/seq_assign.d(31): Error: mismatched sequence lengths, 1 and 2
fail_compilation/seq_assign.d(35): Error: mismatched sequence lengths, 1 and 2
fail_compilation/seq_assign.d(36): Error: cannot implicitly convert expression `__t_field_0` of type `string` to `int`
fail_compilation/seq_assign.d(40): Error: cannot assign `IntString` to expression sequence `(string, int)`
fail_compilation/seq_assign.d(42): Error: cannot implicitly convert expression `AliasSeq!(b, c)` of type `(int, int)` to `IntString`
---
*/

alias Seq(A...) = A;

struct IntString
{
    Seq!(int, string) expand;
    alias this = expand;
}

void main()
{
    int b, c;
    string s;

    Seq!(b,) = 1; // RHS not seq
    Seq!(b,) = Seq!(s,); // b type error
    Seq!(2,) = Seq!(1,); // not lvalue
    Seq!(b,) = Seq!(1, 2); // too many

    auto t = Seq!("two", 3);
    Seq!(s, b) = t; // OK
    Seq!(b,) = t; // too many
    Seq!(b, c) = t; // b type error

    IntString t2;
    Seq!(b, s) = t2; // OK
    t = t2; // type mismatch
    t2 = Seq!(b, s); // OK
    t2 = Seq!(b, c); // c wrong type
}
