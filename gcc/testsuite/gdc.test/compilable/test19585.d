// https://issues.dlang.org/show_bug.cgi?id=19585
struct S19585
{
    M2 stdin;
}

mixin template Handle(T, T invalid_value = T.init) {}

struct M1 { mixin Handle!(size_t); }
struct M2 { mixin Handle!(M1); }
