// https://issues.dlang.org/show_bug.cgi?id=18584

struct S {
    int n;
    auto fun() { return tmp!(a => n)(); }
}

struct tmp(alias fns) {
    alias fun = fns!int;
}

