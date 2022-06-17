// PR c++/103291
// { dg-additional-options -fpic }
// { dg-final { scan-assembler-not "@GOTPCREL" } }
// { dg-require-effective-target fpic }

#pragma GCC visibility push(hidden)

int hidden_fetch(void) {
  extern const int hidden_global;
  return hidden_global;
}
