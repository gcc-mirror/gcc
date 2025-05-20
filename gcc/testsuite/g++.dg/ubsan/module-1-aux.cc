// PR c++/98735

int g(int x) {
  if (x > 0)
    return x - 5;
}

import X;

int main() {
  f(123);
}
