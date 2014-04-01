// PR c++/60708
// { dg-do compile { target c++11 } }

template <class T, class U> struct mypair {
  mypair(T, U) {}
};

template<typename T> struct S {
 mypair<T *, int> get_pair() noexcept {
   return mypair<T*,int>(nullptr, 0);
 }
};

static void foo(const mypair<char *, int> (&a)[2]) noexcept { }

int main()
{
  S<char> s;
  foo({s.get_pair(), s.get_pair()});
}
