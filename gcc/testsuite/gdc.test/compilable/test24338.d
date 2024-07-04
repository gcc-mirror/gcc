// https://issues.dlang.org/show_bug.cgi?id=24338

enum Foo: char[4]
{
    elem = "test"
}

immutable a = [Foo.elem];
immutable b = [Foo.elem];
immutable c = a ~ b;
