// PR c++/115798
// { dg-additional-options "-fmodules-ts" }

import xstd;
import base;

int main() {
  static_assert(__is_same(int8_t, std::int8_t));
}
