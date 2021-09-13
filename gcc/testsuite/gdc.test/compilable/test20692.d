// https://issues.dlang.org/show_bug.cgi?id=20692

struct S() {
    void fun() {
        gun("");
    }
    void gun(T)(T) {
        alias buggy = bug;
    }
}

alias X = S!();

void main() {
    X().gun(0);
}

alias bug =  __traits(getMember, X, "fun");
