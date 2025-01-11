// { dg-additional-options "-fmodules" }

#include "dguide-6.h"
import M;

int main() {
  S a(1);
  S<int> a_copy = a;

  S b(2, 3);
  S<double> b_copy = b;
}
