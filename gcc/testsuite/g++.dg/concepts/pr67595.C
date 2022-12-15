// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts-ts" }

template <class X> concept bool allocatable = requires{{new X}->X *; };
template <class X> concept bool semiregular = allocatable<X>;
template <class X> concept bool readable = requires{requires semiregular<X>;};
template <class> int weak_input_iterator = requires{{0}->readable;};
template <class X> bool input_iterator{weak_input_iterator<X>}; // { dg-prune-output "narrowing conversion" }
template <class X> bool forward_iterator{input_iterator<X>};
template <class X> bool bidirectional_iterator{forward_iterator<X>};
template <class X>
concept bool random_access_iterator{bidirectional_iterator<X>}; // { dg-error "constant" }
void fn1(random_access_iterator);
int main() { fn1(0); }  // { dg-error "" }
