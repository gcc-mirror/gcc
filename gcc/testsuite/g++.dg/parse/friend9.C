// PR c++/19200

struct S {
  struct T{};
  friend void S(T);
};
