// { dg-do run }                                                            
// { dg-options "-O2 -fdump-tree-optimized" }                                   
// { dg-require-effective-target c++11 }

#include <string.h>
#include <iostream>
#include <map>
namespace {
  struct Buf {
    char * buf; int a{0}; int b{0};
    Buf(char * b) : buf(b) { }
    void add(int v) {
      ::memcpy(buf, &v, sizeof(v));
      a += sizeof(v);
      b += sizeof(v);
    }
  };
  struct A {
    virtual void fill(Buf &buf) {
      buf.add(type());
      buf.add(type());
    }
    virtual ~A() {}
    virtual int type() = 0;
  };
  struct BA : A {
    void fill(Buf &buf) {
      A::fill(buf);
      buf.add(type());
      buf.add(type());
    }
    int type() final {
      return 1;
    }
  };
  struct CBA final : BA {
  };
  struct CA final : A {
    ::std::map<int, int> m;
    int type() final {
      return 2;
    }
  };
}
int main(int argc, char ** ) {
  char d[1024];
  CBA cba;
  Buf buf(d);
  cba.fill(buf);
  CA ca;
  return 0;
}
// { dg-final { scan-tree-dump-not "__builtin_unreachable" "optimized" } }
