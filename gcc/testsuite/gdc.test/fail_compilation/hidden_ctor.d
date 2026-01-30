/*
TEST_OUTPUT:
---
fail_compilation/hidden_ctor.d(25): Error: constructor `hidden_ctor.B.this(string s)` is not callable using argument types `()`
fail_compilation/hidden_ctor.d(25):        Note: constructor `hidden_ctor.B.this` hides base class constructor `hidden_ctor.A.this`
---
*/

class A {
    int a;

    this() {
        this.a = 1;
    }
}
class B : A {
    string b;

    this(string s) {
        super();
        this.b = s;
    }
}
void main() {
    auto b = new B();
    b = new B("Hi, Mom!");
}
