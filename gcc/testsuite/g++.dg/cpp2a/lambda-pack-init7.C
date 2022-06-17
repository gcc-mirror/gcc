// PR c++/102629
// { dg-do compile { target c++20 } }

template <class T> T&& forward(T&);
template <class T> T&& forward(T&&);

struct S {};

template <typename... Args>
void foo(Args&&... args) {
  [...args = forward<Args> /*(args)*/] { // { dg-error "14:" }
    [](auto...) { } (forward<Args>(args)...);
  };
}

void bar( ) {
  foo(S{});
}
