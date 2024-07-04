// PR c++/115358

template<class T>
struct A { static int STR[]; };

template<class T>
int A<T>::STR[] = {1,2,3};

void f(int(&)[3]);

template<class T>
void g() {
  f(A<int>::STR); // { dg-bogus "int []" }
}
