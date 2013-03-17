// PR c++/55532
// { dg-do compile { target c++11 } }

struct Foo {
    void doit() {
    }
};

template<typename T>
void oops(Foo &foo, const T &) {
    auto fun = [&] () mutable {
        foo.doit();
    };
    auto fun2 = [=]() {
        fun();			// { dg-error "" }
    };
    fun2();
}

int main() {
    Foo foo;
    oops(foo, 1);
}
