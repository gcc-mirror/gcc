struct S {
    @disable this();

    this(int i) {
    }
}

class C {
    this() {
        s = S(1);
    }

    S s;
}

class CR
{
    S s;

    this() {
        s = S(1);
    }
}

void main() {
    auto c = new C;
}
