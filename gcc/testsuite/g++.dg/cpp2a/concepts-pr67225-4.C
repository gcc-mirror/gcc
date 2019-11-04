// PR c++/67225
// { dg-do compile { target c++2a } }
// { dg-additional-options "-fconcepts-ts" }

template <class, class>
concept bool C1 = true;

template <class>
concept bool C2 = requires { { 42 } -> C1<int>; };

int main() {
    class A { int x; } a;
    a.x = 42; // { dg-error "private within this context" }
}
