// PR c++/30274
// { dg-do link }

struct S {
  bool x : 4;
};

S s;

template <typename T>
void f(T);

template <>
void f(bool) {} 

int main() {
  f(s.x++); // { dg-warning "deprecated" "" { target { ! c++1z } } }
  // { dg-error "forbidden" "" { target c++1z } 17 }
  f(++s.x); // { dg-warning "deprecated" "" { target { ! c++1z } } }
  // { dg-error "forbidden" "" { target c++1z } 19 }
}
