struct S {}

class C {
    S s;
    alias s this;
}

void main() {
    auto c = new C;
    auto p = cast(void*) c;
}
