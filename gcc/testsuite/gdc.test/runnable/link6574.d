// PERMUTE_ARGS:
module link6574;

enum Method { A, B, }

int foo(Method method = Method.A)()
{
    static assert(foo.mangleof == "_D8link657428__T3fooVE8link65746Methodi0Z3fooFZi");
    return 10 * foo!method();
}
int foo(Method method : Method.A)()
{
    static assert(foo.mangleof == "_D8link657429__T3fooHVE8link65746Methodi0Z3fooFZi");
    return 2;
}
int foo(Method method : Method.B)()
{
    static assert(0);
    return 3;
}

int bar(Method method = Method.B)()
{
    static assert(bar.mangleof == "_D8link657428__T3barVE8link65746Methodi1Z3barFZi");
    return 10 * bar!method();
}
int bar(Method method : Method.A)()
{
    static assert(0);
    return 2;
}
int bar(Method method : Method.B)()
{
    static assert(bar.mangleof == "_D8link657429__T3barHVE8link65746Methodi1Z3barFZi");
    return 3;
}

void main()
{
    assert(foo!() == 10 * 2);
    assert(foo() == 10 * 2);

    assert(bar!() == 10 * 3);
    assert(bar() == 10 * 3);
}
