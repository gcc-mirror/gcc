module imports.b11447;
class A {}

void map(alias dg)(int b) { }

auto aaa(A a)
{
    int bs;

    static A ggg;

    bs.map!(
        b => (a is ggg ? "a" : "b")
    );
}
