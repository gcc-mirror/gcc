// PR c++/120013

template <typename> struct tuple_element;
template <typename T> tuple_element<T*> get(T);

// This case wasn't an issue for the PR, but worth double-checking
template <typename> constexpr int var = 123;
template <typename T> void foo(T, int = var<T*>);
