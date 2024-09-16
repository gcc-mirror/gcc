// PR c++/109790
// Up to GCC 10, the mangling would be missing the "on" marker, hence be wrong.
// Check that this is fixed with the latest ABI.

// { dg-do compile { target c++11 } }

struct A {
  template<class T> void operator+(T);
};

template<class T>
decltype(&A::operator+<T>) f();

int main() {
  f<int>();
}

// { dg-final { scan-assembler "_Z1fIiEDTadsr1AonplIT_EEv" } } 
