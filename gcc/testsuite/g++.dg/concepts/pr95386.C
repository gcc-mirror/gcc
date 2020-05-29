// PR c++/95386
// { dg-do compile { target concepts } }

template <typename> struct blah {
 typedef bool value_type;
 constexpr operator value_type() { return false; }
};

template <class T> void fn1(T) requires (!blah<T>());

void fn2() { fn1(0); }
