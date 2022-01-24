// DR 1835

template <class T> void f(T t) { t.foo::bar(); }
struct foo { void bar(); };
struct baz : foo { };
int main() { f(baz()); }
