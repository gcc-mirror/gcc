// Build don't link:

class foo {
  friend void bar<int>(int); // ERROR - must be declared first
};

template <typename T> void bar(T);
