// PR c++/64487

struct foo {
      int member;
};

template < int N>
struct bar {};

template <int N>
struct qux {
        static bar<N+__builtin_offsetof(foo,member)> static_member;
};

template <int N>
bar<N+__builtin_offsetof(foo,member)> qux<N>::static_member;

int main() { }
