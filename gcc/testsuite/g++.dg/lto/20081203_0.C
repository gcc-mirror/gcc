extern void f();
extern void g();
struct Foo { static inline void Bar() { f(); } };
static void Func() { Foo::Bar(); }
int main() { g (); Func(); return 0; }
