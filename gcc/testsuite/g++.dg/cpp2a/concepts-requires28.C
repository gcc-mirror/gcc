// PR c++/101677
// { dg-do compile { target c++20 } }

template<class T>
concept C_bug_with_forward_decl = requires(T& t){
    t.template f<class S>();
};

struct good {
    template<class T> void f() {}
};

static_assert(C_bug_with_forward_decl<good>);
