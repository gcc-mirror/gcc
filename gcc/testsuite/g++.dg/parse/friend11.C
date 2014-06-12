// PR c++/19200

struct S {
  struct T{};
  template<typename> friend void S(T);
};
