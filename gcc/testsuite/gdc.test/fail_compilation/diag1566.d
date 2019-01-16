/*
TEST_OUTPUT:
---
fail_compilation/diag1566.d(23): Error: multiple ! arguments are not allowed
fail_compilation/diag1566.d(24): Error: multiple ! arguments are not allowed
fail_compilation/diag1566.d(25): Error: multiple ! arguments are not allowed
fail_compilation/diag1566.d(26): Error: multiple ! arguments are not allowed
fail_compilation/diag1566.d(28): Error: multiple ! arguments are not allowed
fail_compilation/diag1566.d(29): Error: multiple ! arguments are not allowed
---
*/

template T(int n)
{
    template T(char c)
    {
        alias long T;
    }
}

void main()
{
    static assert(is(long == T!(3)!('b')));
    static assert(is(long == T! 3 ! 'b' ));
    static assert(is(long == T!(3)! 'b' ));
    static assert(is(long == T! 3 !('b')));

    static assert(is(long == T!(3)! 'b' !"s"));
    static assert(is(long == T! 3 !('b')!"s"));
}
