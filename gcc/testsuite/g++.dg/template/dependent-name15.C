// { dg-do compile { target c++11 } }
template <int N> struct A { static void foo(){} };
template <> struct A<sizeof(char)> { using foo = int; };

template <class T> void f(T t1) { 
    A<sizeof(t1)>::foo();
}

template <class T> void g(T t2) { 
    /* if the comparing_specializations check breaks in cp_tree_equal
    case PARM_DECL, the error will incorrectly report A<sizeof (t1)> */
    A<sizeof(t2)>::foo(); // { dg-error "dependent-name .A<sizeof .t2.>::foo" }
}

void h() {
    f(0);
    g('0');
}
