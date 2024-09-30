// PR c++/115231
// { dg-additional-options "-fmodules-ts" }

import M;

template <typename>
struct U;

template <template <typename> typename TT, typename Inner>
struct U<TT<Inner>> {
  void go() {
    TT t(10);
  }
};

int main() {
  U<decltype(f())>{}.go();
  U<decltype(g())>{}.go();
}
