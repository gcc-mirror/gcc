// PR c++/60605

template <typename T = int>
struct Foo {
    void bar() {
        void bug();
    }
};
