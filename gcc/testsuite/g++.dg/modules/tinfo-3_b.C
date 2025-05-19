// PR c++/120350
// { dg-additional-options "-fmodules" }

import "tinfo-3_a.H";

int main() {
  return tinfo == typeid(int);
}
