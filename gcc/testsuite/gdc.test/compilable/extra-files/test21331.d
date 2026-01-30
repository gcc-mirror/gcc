struct S(alias fun) {
    int foo() {
        int r = fun(0);
        static foreach (i; 0..2)
            r += (x => 2)(0);
        return r;
    }
}

int bar() {
    int r;
    static foreach (i; 0..2)
        r += S!(x => 1)().foo();
    return r;
}

// nm test21331.o | grep __lambda_L5_C19
