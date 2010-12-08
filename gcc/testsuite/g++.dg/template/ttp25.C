// { dg-do compile }
template<typename T, template<T> class C>
void f1(T, C<5>);		// { dg-message "note" }

template<typename T, template<T> class C>
void f2(C<5>, T);

template<typename T, template<T> class C>
void f3(C<5>, T);		// { dg-message "note" }

template<typename T> struct metafun { typedef T type; };

template<> struct metafun<short> { typedef int type; };

template<typename T, template<typename metafun<T>::type> class C>
void f4(T, C<5>);		// { dg-message "note" }

template<int N> struct X {};
void g() {
  f1(5l, X<5>()); // { dg-error "no matching" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 20 }
  f2(X<5>(), 5);
  f3(X<5>(), 5l); // { dg-error "no matching" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 23 }
  f4(5, X<5>());
  f4(5l, X<5>()); // { dg-error "no matching" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 26 }
  f4((short)5, X<5>());
}
