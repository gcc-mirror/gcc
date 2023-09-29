// PR c++/99599
// { dg-do compile { target c++20 } }

struct foo_tag { };
struct bar_tag { };

template<class T>
concept fooable = requires(T it) { invoke_tag(foo_tag{}, it); };

template<class T> void invoke_tag(foo_tag, T);
template<class T> void invoke_tag(bar_tag, T) requires fooable<T>;

int main() {
  invoke_tag(foo_tag{}, 0);
  invoke_tag(bar_tag{}, 0);
}
