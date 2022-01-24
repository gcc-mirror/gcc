// https://issues.dlang.org/show_bug.cgi?id=19476

mixin template operators() {
    int foo(int op = 1, T)(T rhs) {
        return 1;
    }
}

struct S {
    mixin operators ops;
    int foo(int op = 1, T)(T a) {
        return ops.foo!1(a);
    }
}

void main() {
    S.init.foo(S.init);
}
