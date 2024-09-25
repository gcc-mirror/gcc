// PR c++/53288
// { dg-do run { target c++11 } }

struct B {
   B(int data) : _data(data) { }
   ~B() { }

   int _data;

private:
   B() = delete;
   B(const B &) = delete;
   B(B &&) = delete;
};

int c,d;
struct A {
   B b;
   A(int data) : b(data) { ++c; }
   ~A() { ++d; }

private:
   A() = delete;
   A(const A &) = delete;
   A(A &&) = delete;
};

template <class T>
void f(T t) {
  const B &b = A(1).*t;
  if (d) __builtin_abort ();
}

int main() {
  const B &b = A(1).*(&A::b);
  if (d) __builtin_abort ();

  f(&A::b);
}
