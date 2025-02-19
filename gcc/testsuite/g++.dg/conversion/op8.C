// PR c++/118282
// { dg-do "compile" }

#if __cplusplus >= 201103L
# include <cstdint> // Only available from c++11 onwards.
#endif

struct A {
  explicit A (int);
  operator void* () const;
};

struct B {
  explicit B (int);
  operator char* () const;
};

struct C {
  explicit C (int);
  operator int () const;
};

struct BothWays {
  BothWays (int);
  operator void*() const;
};

extern bool my_bool;

void foo (const A& a, const B& b, const C& c, const BothWays& d) {
  void *res_a_1 = 0	  ? 0 : a;
  void *res_a_2 = 1	  ? 0 : a;
  void *res_a_3 = my_bool ? 0 : a;
  void *res_a_4 = 0	  ? a : 0;
  void *res_a_5 = 1	  ? a : 0;
  void *res_a_6 = my_bool ? a : 0;

  void *res_b_1 = 0	  ? 0 : b;
  void *res_b_2 = 1	  ? 0 : b;
  void *res_b_3 = my_bool ? 0 : b;
  void *res_b_4 = 0	  ? b : 0;
  void *res_b_5 = 1	  ? b : 0;
  void *res_b_6 = my_bool ? b : 0;

  //
  // 0 valued constants that are NOT null pointer constants - this worked already.
  //
  char zero_char  = 0;
  void *res_ko1	  = 0	  ? zero_char : a; // { dg-error "different types" }

#if __cplusplus >= 201103L
  // Those are only available starting with c++11.
  int8_t zero_i8  = 0;
  void *res_ko2	  = 0	  ? zero_i8   : a; // { dg-error "different types" "" { target c++11 }  }
  uintptr_t zerop = 0;
  void *res_ko3	  = 0	  ? zerop     : a; // { dg-error "different types" "" { target c++11 }  }
#endif

  // Conversion to integer - this worked already.
  int res_int	  = 0	  ? 0 : c;

  // Case where one arm is of class type that can be constructed from an
  // integer and the other arm is a null pointer constant (inspired by
  // g++.dg/template/cond5.C).
  0 ? d : 0;
  0 ? 0 : d;
}

int main(){
  A a (5);
  B b (42);
  C c (43);
  BothWays d (1982);
  foo (a, b, c, d);
}
