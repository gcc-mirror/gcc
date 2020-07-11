// PR c++/94926
// { dg-do compile { target c++11 } }

template<typename>
struct A {
    static auto self_reference = A{}; // { dg-error "incomplete" }
};

int main() {
    A<void>{};
}
