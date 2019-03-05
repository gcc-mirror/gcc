/*
TEST_OUTPUT:
----
fail_compilation/ice12727.d(16): Error: alias ice12727.IndexTuple!(1, 0).IndexTuple recursive alias declaration
fail_compilation/ice12727.d(23): Error: template instance ice12727.IndexTuple!(1, 0) error instantiating
fail_compilation/ice12727.d(27):        instantiated from here: Matrix!(float, 3)
fail_compilation/ice12727.d(28):        instantiated from here: Vector!(float, 3)
----
*/

template IndexTuple(int e, int s = 0, T...)
{
    static if (s == e)
        alias IndexTuple = T;
    else
        alias IndexTuple = IndexTuple!(e);
}

struct Matrix(T, int N = M)
{
    pure decomposeLUP()
    {
        foreach (j; IndexTuple!(1)) {}
    }
}

alias Vector(T, int M) = Matrix!(T, M);
alias Vector3 = Vector!(float, 3);
