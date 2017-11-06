// { dg-do compile }
// { dg-options "-O2 -fstrict-aliasing" }
// { dg-additional-options "-Wno-return-type" }

// This caused an ICE during placement new.

namespace Pooma {
   typedef int Context_t;
   namespace Arch {
   }
   inline Context_t context() {
  }
   inline int contexts() {
  }
 }
template<class DomT, class T, class NewDom1T> struct DomainTraitsScalar {
  };
template<class T> struct DomainTraits : public DomainTraitsScalar<T, T, T> {
  };
template<int Dim> class Grid;
template<class DT> class DomainBase {
  };
template<int Dim, class DT> class Domain : public DomainBase<DT> {
  };
#include <vector>
template<> class Grid<1> : public Domain<1, DomainTraits<Grid<1> > > {
  };
namespace Pooma {
 class PatchSizeSyncer {
    typedef Grid<1> Grid_t;
    PatchSizeSyncer(int contextKey, Grid_t &localGrid);
    int myContext_m;
    int numContexts_m;
    int localKey_m;
    Grid_t localGrid_m;
    typedef std::pair<int,Grid_t *> Elem_t;
    std::vector<Elem_t> gridList_m;
  };
 }
namespace Pooma {
 PatchSizeSyncer::PatchSizeSyncer(int contextKey, Grid_t &localGrid)   :
myContext_m(Pooma::context()),     numContexts_m(Pooma::contexts()),    
localKey_m(contextKey),     localGrid_m(localGrid) {
    if (myContext_m == 0) gridList_m.reserve(numContexts_m);
  }
 }
