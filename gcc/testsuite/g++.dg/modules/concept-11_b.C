// PR c++/119939
// { dg-additional-options "-fmodules -std=c++20" }

import "concept-11_a.H";

int main() {
  S<int> s;
  s == s;
}
