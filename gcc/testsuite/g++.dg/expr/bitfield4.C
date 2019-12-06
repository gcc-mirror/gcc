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
  f(s.x++); // { dg-warning "7:use of an operand of type .bool. in .operator\\+\\+. is deprecated" "" { target { ! c++17 } } }
  // { dg-error "forbidden" "" { target c++17 } .-1 }
  f(++s.x); // { dg-warning "9:use of an operand of type .bool. in .operator\\+\\+. is deprecated" "" { target { ! c++17 } } }
  // { dg-error "forbidden" "" { target c++17 } .-1 }
}
