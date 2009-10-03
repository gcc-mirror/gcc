void f () {}
struct Foo { static inline void Bar() { f(); } };
static void Func() { Foo::Bar(); }
void g () { Func (); }
