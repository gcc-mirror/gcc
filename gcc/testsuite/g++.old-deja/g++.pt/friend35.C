// { dg-do assemble  }

class foo {
  friend void bar<int>(int); // { dg-error "" } must be declared first
};

template <typename T> void bar(T);
