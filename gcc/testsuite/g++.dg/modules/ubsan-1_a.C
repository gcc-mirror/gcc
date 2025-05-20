// PR c++/98735
// { dg-additional-options "-fmodules -fsanitize=undefined -Wno-return-type" }
// { dg-module-cmi X }

export module X;

export inline int f(int x) {
    if (x > 0)
      return x * 5;
}
