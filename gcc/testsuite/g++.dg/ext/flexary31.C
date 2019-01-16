// PR c++/88222
// { dg-options -Wno-pedantic }

typedef char a[];

class S {
  a : 4; // { dg-error "bit-field" }
};
