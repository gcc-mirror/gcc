// PR c++/106820
// { dg-additional-options "-fmodules-ts" }

import "pr106820_a.H";

int main() {
  __gthrw___pthread_key_create();
}
