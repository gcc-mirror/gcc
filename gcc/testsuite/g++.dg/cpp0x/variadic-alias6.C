// PR c++/105003
// { dg-do compile { target c++11 } }

template <class T> struct A;
template <class T, class U> struct B { };
template <class... Ts> struct C { };

// Fn is not complex, since T is used outside the pack expansion, so the two
// partial specializations are equivalent.
template <class T, class... Ts> using Fn = T(Ts...);
template <class T, class... Ts> struct A<Fn<T,Ts...>*> { };
template <class T, class... Ts> struct A<T(*)(Ts...)> { }; // { dg-error "redefinition" }

// CB is complex, since T is only used in the pack expansion, so the two
// partial specializations are functionally equivalent but not equivalent.
template <class T, class ...Ts> using CB = C<B<T,Ts>...>;
template <class T, class ...Ts> struct A<CB<T,Ts...>*> { };
template <class T, class ...Ts> struct A<C<B<T,Ts>...>*> { }; // IFNDR
A<C<B<int,int>>*> a;		// { dg-error "ambiguous" }
// { dg-prune-output "incomplete" }
