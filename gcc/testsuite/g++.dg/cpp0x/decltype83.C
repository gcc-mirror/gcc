// PR c++/109790
// This used to work until GCC 10; force the usage of ABI 15 (the highest
// usable in GCC 10) and check that the mangling (actually wrong; see
// decltyp83a.C) matches that of GCC 10's default ABI version (14).

// { dg-do compile { target c++11 } }
// { dg-additional-options "-fabi-version=15" }

struct A {
  template<class T> void operator+(T);
};

template<class T>
decltype(&A::operator+<T>) f();

int main() {
  f<int>();
}

// { dg-final { scan-assembler "_Z1fIiEDTadsr1AplIT_EEv" } }
