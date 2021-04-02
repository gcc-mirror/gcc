// PR c++/96960
// { dg-do compile { target c++20 } }

template <class, class> concept C0 = true;

template <class T>
concept C = requires(T t) {
  { 42 } -> C0<char [([] { return 42; }())]>;
};

static_assert(C<int>);

C0<char [([] { return 42; }())]> auto x = 42;

int f(C0<char [([] { return 42; }())]> auto x);
int y = f(42);
