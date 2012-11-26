// PR c++/55137
// s should have constant initialization.
// { dg-final { scan-assembler-not "GLOBAL" } }

struct S {
  int b;
};

struct S s = { -1 + (int)(sizeof(int) - 1) };
