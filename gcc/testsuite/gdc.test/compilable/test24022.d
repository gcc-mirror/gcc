// https://issues.dlang.org/show_bug.cgi?id=24022
// EXTRA_FILES: imports/imp24022.c
import imports.imp24022;

auto some_d_func(E v) {
    return v;
}

auto some_d_other_func() {
    const struct R {
        E r;
        this(in E vparam) { r = vparam; }
    }
    return R(A);
}

void main(string[] args) {
    E expected = E.A;
    E res = some_d_func(A);
    assert (res == A);
    assert (res == expected);

    res = some_d_func(E.B);
    assert (res == B);
    assert (res == E.B);

    auto res2 = some_d_other_func();
    assert (res2.r == A);
    assert (res2.r == expected);
}
