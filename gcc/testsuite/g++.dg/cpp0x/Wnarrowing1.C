// PR c++/53159
// { dg-do compile { target c++11 } }
// { dg-options "-Wnarrowing -Wno-overflow" }

struct X
{
  constexpr operator int() { return __INT_MAX__; }
};

int f() { return __INT_MAX__; }

signed char a { __INT_MAX__ };     // { dg-error "narrowing conversion" }
signed char b { f() };             // { dg-warning "narrowing conversion" }
signed char c { X{} };             // { dg-error "narrowing conversion" }

signed char ar[] { __INT_MAX__ };  // { dg-error "narrowing conversion" }
signed char br[] { f() };          // { dg-warning "narrowing conversion" }
signed char cr[] { X{} };          // { dg-error "narrowing conversion" }
