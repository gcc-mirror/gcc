// PR c++/70790
// { dg-do compile { target c++11 } }
// { dg-additional-options -fabi-compat-version=0 }

template<bool B>
struct A { };

template<class T>
void f(A<noexcept(T{})>);

int main() {
  f<int>({});
}

// { dg-final { scan-assembler "_Z1fIiEv1AIXnxtlT_EEE" } }
