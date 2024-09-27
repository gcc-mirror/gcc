[[deprecated]] int foo () { return 42; }
template <int N>
struct S {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
static int bar () { return foo (); }
#pragma GCC diagnostic pop
};
