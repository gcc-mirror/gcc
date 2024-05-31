// PR c++/67225
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts" }

template <class, class>
concept C1 = true;

template <class>
concept C2 = requires { { 42 } -> C1<int>; };

int main() {
    class A { int x; } a;
    a.x = 42; // { dg-error "private within this context" }
}
