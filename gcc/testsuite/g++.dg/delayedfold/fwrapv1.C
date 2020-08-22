// PR c++/69631
// { dg-options -fwrapv }
// { dg-skip-if "" int16 }

struct C {
  static const unsigned short max = static_cast<unsigned short>((32767 * 2 + 1));
};
