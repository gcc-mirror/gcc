// { dg-do link  }
// Origin: Mark Mitchell <mark@codesourcery.com>

namespace N {
  extern "C" int i;

  void f () {
    i = 3;
  }
}

int i;

int main () { N::f (); }
