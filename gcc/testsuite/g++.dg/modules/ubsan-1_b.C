// PR c++/98735
// { dg-additional-options "-fmodules -fsanitize=undefined -Wno-return-type" }
// Note: can't work out how to do a link test here.

int g(int x) {
  if (x > 0)
    return x - 5;
}

import X;

int main() {
  f(123);
}
