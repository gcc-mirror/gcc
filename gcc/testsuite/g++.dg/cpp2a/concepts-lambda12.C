// PR c++/92633
// { dg-do compile { target c++20 } }

template<class A, class B>
concept different_than = !__is_same_as(A, B);

template<class B>
auto diff(B) {
    return [](different_than<B> auto a) {};
}

int main() {
    diff(42)("");
    diff(42)(42); // { dg-error "no match" }
}
