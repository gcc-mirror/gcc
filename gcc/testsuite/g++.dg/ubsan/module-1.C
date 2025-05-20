// PR c++/98735
// { dg-do run { target c++17 } }
// { dg-options "-fmodules -fsanitize=undefined -Wno-return-type" }
// { dg-additional-sources module-1-aux.cc }

export module X;

export inline int f(int x) {
    if (x > 0)
      return x * 5;
}
