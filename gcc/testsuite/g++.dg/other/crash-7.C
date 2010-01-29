// Origin: PR c++/42336
// { dg-options "-std=c++0x -O2 -g" }
// { dg-do compile }

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


