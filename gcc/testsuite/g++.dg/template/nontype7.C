// { dg-do compile }
// Origin: C++ standard, [temp.arg.nontype]/2

template<class T, char* p> struct X {
  X();
  X(const char* q) { /* ... */ }
};

char p[] = "Vivisectionist";

X<int,"Studebaker"> x1;    // { dg-error "string literal" }
X<int, p> x2;

// { dg-bogus "" "additional errors" { xfail *-*-* } 11 }

