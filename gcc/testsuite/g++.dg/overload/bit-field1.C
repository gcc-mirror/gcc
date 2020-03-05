// PR c++/92859
// { dg-do compile { target c++11 } }

void f(int) = delete;

struct ES { 
  enum E { v }; 
  friend void f(E) { }
};

struct S {
  ES::E e : 1; 
};

int main() {
  S s{}; 
  f (s.e);
}
