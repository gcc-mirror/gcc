// { dg-do compile }
// { dg-options "-O2 -Wextra -Wall -Warray-bounds" }

struct SparseBitVectorElement {
  long Bits[2];
  int find_first() const;
};

// we should not get an `array subscript 2 is above array bounds of`
// warning here because we have an unreachable at that point

int SparseBitVectorElement::find_first() const {
  for (unsigned i = 0; i < 2; ++i)
    if (Bits[i]) // { dg-bogus "is above array bounds of" }
      return i;
  __builtin_unreachable();
}
