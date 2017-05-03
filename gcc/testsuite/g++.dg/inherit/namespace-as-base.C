// { dg-do compile }

namespace Out {
  namespace In {
  }
}

class Klasse : public Out::In {  // { dg-error ".*" }
};
