// PR c++/41611
// { dg-final { scan-assembler-not "_ZGVZN1A1fEvE1i" } }

struct A {
  static int f()
  {
    static int &i = *new int();
    return i;
  }
};
