// { dg-do compile }

struct S {
        S() {}
};
S f() {
        static S s;
        return s;
}
