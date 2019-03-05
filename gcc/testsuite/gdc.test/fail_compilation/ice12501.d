/*
TEST_OUTPUT:
---
fail_compilation/ice12501.d(29): Error: function ice12501.foo (int value) is not callable using argument types (int, int)
fail_compilation/ice12501.d(29): Error: function ice12501.foo (int value) is not callable using argument types (int, int)
fail_compilation/ice12501.d(43): Error: template instance ice12501.reduce!(foo, foo).reduce!(Tuple!(int, int), int[]) error instantiating
---
*/

struct Tuple(T...)
{
    alias Types = T;
    T field;
    alias field this;
}
Tuple!A tuple(A...)(A args) { return typeof(return)(args); }

template reduce(fun...)
{
    auto reduce(Args...)(Args args)
    {
        alias seed = args[0];
        alias r    = args[1];
        Args[0] result = seed;
        for (; r.length != 0; r = r[1..$])
        {
            foreach (i, Unused; Args[0].Types)
            {
                result[i] = fun[i](result[i], r[0]);
            }
        }
        return result;
    }
}

int foo(int value)
{
    return value;
}

void main()
{
    reduce!(foo, foo)(tuple(0, 0), [ 1 ]);
}
