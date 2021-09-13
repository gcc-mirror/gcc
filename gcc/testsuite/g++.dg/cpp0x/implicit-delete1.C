// PR c++/100644
// { dg-do compile { target c++11 } }

struct NonMovable {
  NonMovable(NonMovable&&) = delete;
};

template <class T>
struct Maybe {
  NonMovable mMember;

  template <typename U>
  Maybe(Maybe<U>&&);
};

void foo(Maybe<int>);

void unlucky(Maybe<int>&& x) {
  Maybe<int> var{(Maybe<int>&&)x};
}
