// { dg-do compile }

// Origin: Wolfgang Bangerth <bangerth@ticam.utexas.edu>

// PR c++/9022, nested namespace in using declaration

namespace gnu {
  namespace gcc {
  }
}
using gnu::gcc;		// { dg-error "namespace" }
