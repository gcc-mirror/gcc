// https://issues.dlang.org/show_bug.cgi?id=17145

auto tuple(T...)(T t) {
    struct Result {
        T expand;
    }
    return Result(t);
}

void baz()
{
    enum zoo = tuple(1, 2).expand; // Error: value of __tup1847 is not known at compile time
}
