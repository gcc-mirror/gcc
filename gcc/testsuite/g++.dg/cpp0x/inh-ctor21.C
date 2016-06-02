// PR c++/70972
// { dg-do run { target c++11 } }

struct abort_on_copy{
    abort_on_copy(abort_on_copy&&) = default;
    abort_on_copy(const abort_on_copy&) { __builtin_abort(); }
    abort_on_copy() = default;
};

struct A {
    A(abort_on_copy) {}
};
struct B : A {
    using A::A;
};

int main() {
    B b(abort_on_copy{});
}
