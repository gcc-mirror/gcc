// PR c++/67835
// { dg-do compile { target c++14 } }

template<class Tag, class T>
auto g(Tag tag, T x) {
 return f(tag, x);
}

namespace abc {
struct tag {};

struct A {};

template<class T>
auto f(tag, T x) { return x; }
}

int main() {
 g(abc::tag(), abc::A());
 return 0;
}
