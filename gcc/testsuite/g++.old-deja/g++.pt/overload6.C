// Check that foo<int> isn't resolved too early.

template <class T> void foo(T*);
template <class T, class U> void foo(T*, U) { }

template <class T, class U> void bar(void (*)(T, U), U) { }

int main() {
  bar<int*>(&foo, 1);
  bar<int*>(&foo<int>, 1);  
  bar<int*>(foo, 1);
  bar<int*>(foo<int>, 1);  
}
