// PR c++/83020
// { dg-do compile { target c++17 } }

struct NoDefault {
    int val = 1234;
    NoDefault(int v) : val(v) {}
};
template <class T>
struct Whoops {
    const char *str;
    T obj;
    Whoops(const char *s, T v = T()) : str(s), obj(v) {}  // { dg-error "no matching" }
};
const char *test() {
    return Whoops<NoDefault>("hi").str;
}
