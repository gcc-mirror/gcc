// { dg-do run  }
// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@dcc.unicamp.br>
// based on bug report by Fredrik Öhrström <d92-foh@nada.kth.se>

#include <cstdlib>

using namespace std;

struct vbase {
  virtual int get_a() const = 0;
};

struct base: virtual vbase {
  int a;
  base(int aa) : a(aa) {}
  int get_a() const { return a; }
};

struct mid: base {
  mid(int bb) : base(bb) {
    // when mid is not in charge of vbase initialization,
    // a derived-aware vtable is needed for vbase
    if (((vbase*)this)->get_a() != bb)
      abort();
  }
};

struct derived: virtual mid {
  derived(int cc) : mid(cc) {}
};

int main () {
  derived(1);
}
