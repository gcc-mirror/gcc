// { dg-do compile }

namespace Out {
  namespace In {
  }
}

class Class : public Out::In {  // { dg-error ".*" "" { xfail *-*-* } }
};
