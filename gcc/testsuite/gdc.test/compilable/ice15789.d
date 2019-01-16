// REQUIRED_ARGS: -o-
// PERMUTE_ARGS:

struct InputRange {}

auto md5OfA(T...)(T ) {}
auto md5OfB(T...)(T ) {}

template fqnSymA(alias T : X!A, alias X, A...)
{
    template fqnTuple(B) { enum fqnTuple = 1; }
    enum fqnSymA = fqnTuple!A;
}
template fqnSymB(alias T : X!A, alias X, A...)
{
    template fqnTuple(B) { enum fqnTuple = 1; }
    enum fqnSymB = fqnTuple!A;
}

void test1()    // OK <- NG
{
    md5OfA(InputRange());
    auto n = fqnSymA!(md5OfA!InputRange);
}

void test2()    // OK
{
    auto n = fqnSymB!(md5OfB!InputRange);
    md5OfB(InputRange());
}
