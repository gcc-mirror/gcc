// { dg-do compile { target c++11 } }
template <int N> struct A { static void foo(){} };
template <> struct A<sizeof(char)> { using foo = int; };

template<class T1> auto f(T1 t1) -> decltype(A<sizeof(t1)>::foo());

/* if the comparing_specializations check breaks in cp_tree_equal
case PARM_DECL, the error will incorrectly report A<sizeof (t1)> */
template<class T2> auto g(T2 t2) -> decltype(A<sizeof(t2)>::foo()); // { dg-error "dependent-name .A<sizeof .t2.>::foo" }

void h() {
    f(0);
    g('0'); // { dg-error "no matching function" }
}
