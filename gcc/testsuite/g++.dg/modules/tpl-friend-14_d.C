// { dg-additional-options "-fmodules-ts" }

import X;
import Y;

int main() {
  A<long> a;
  B<int>::type r = 10;
}
