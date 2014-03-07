// Origin: PR c++/42336
// { dg-options "-O2 -g" }
// { dg-do compile { target c++11 } }

struct X {
      void func() {}
};

template<typename T, void (X::*P)() = &T::func>
void b(T) {}

int main() {
      b(X()); /* line 9 */
        X().func();

          return 0;
}


