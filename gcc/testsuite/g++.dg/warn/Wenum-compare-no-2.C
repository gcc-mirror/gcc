// PR c++/53524
// { dg-options "-Wno-enum-compare" }

template < typename > struct PointerLikeTypeTraits {
  enum { NumLowBitsAvailable };
};

class CodeGenInstruction;
class CodeGenInstAlias;

template < typename T>
struct PointerIntPair {
  enum { IntShift = T::NumLowBitsAvailable };
};

template < typename PT1, typename PT2 > struct PointerUnionUIntTraits {
  enum {
    PT1BitsAv = PointerLikeTypeTraits < PT1 >::NumLowBitsAvailable,
    PT2BitsAv = PointerLikeTypeTraits < PT2 >::NumLowBitsAvailable,
    NumLowBitsAvailable = 0 ? PT1BitsAv : PT2BitsAv
  };
};

template < typename PT1, typename PT2 > class PointerUnion {
  typedef PointerIntPair < PointerUnionUIntTraits < PT1, PT2 > > ValTy;
  ValTy Val;
};

struct ClassInfo {
  PointerUnion < CodeGenInstruction *, CodeGenInstAlias * > DefRec;
};
