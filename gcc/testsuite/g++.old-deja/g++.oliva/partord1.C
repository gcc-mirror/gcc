// { dg-do assemble  }

// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@dcc.unicamp.br>

template <typename T> void foo(T);
template <typename T> void foo(T*);

template <typename T> class bar {
 private:
  int i; // { dg-error "" } this variable
  friend void foo<T>(T);
};

template <typename T> void foo(T) {
  bar<T>().i = 0; // ok, I'm a friend
}
template <typename T> void foo(T*) {
  bar<T*>().i = 1; // { dg-error "" } not a friend
}

int main() {
  int j = 0;
  foo(j); // calls foo<int>(int), ok
  foo(&j); // calls foo<int>(int*)
  foo<int*>(&j); // calls foo<int*>(int*), ok
}
