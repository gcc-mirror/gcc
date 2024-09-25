// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<class T, class U> concept same_as = __is_same(T, U);
template <class X> concept allocatable = requires{{new X}->same_as<X *>; };
template <class X> concept semiregular = allocatable<X>;
template <class X> concept readable = requires{requires semiregular<X>;};
template <class> int weak_input_iterator = requires{{0}->readable;};
template <class X> bool input_iterator{weak_input_iterator<X>}; // { dg-prune-output "narrowing conversion" }
template <class X> bool forward_iterator{input_iterator<X>};
template <class X> bool bidirectional_iterator{forward_iterator<X>};
template <class X>
concept random_access_iterator = bidirectional_iterator<X>; // { dg-error "constant" }
void fn1(random_access_iterator auto);
int main() { fn1(0); }  // { dg-error "" }
