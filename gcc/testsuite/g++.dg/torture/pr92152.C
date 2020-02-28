/* { dg-do run } */
using size_t = decltype (sizeof (0));
using uint64_t = unsigned long long;

namespace HPHP {

inline void bitvec_set(uint64_t* bits, size_t index) {
  bits[index / 64] |= 1ull << (index % 64);
}

namespace jit {
///////////////////////////////////////////////////////////////////////////////

struct VregSet {
  struct Block;

  VregSet() : blocks{} { blocks.data[1] = 1LL << 0x30; };

  VregSet(const VregSet& o)
    : blocks{o.blocks}
  {
  }

  bool any() const {
    if (!isExtended()) return blocks.any();
    return true;
  }
  void removePhys() {
    auto const b = !isExtended() ? &blocks : extended.blocks;
    b->data[0] = 0;
    b->data[1] = 0;
  }
  static constexpr size_t kBitsPerBlock = 256;
  bool isExtended() const {
    return extended.data[1] & (1ULL << (kExtendedBit % 64));
  }

  static constexpr size_t kExtendedBit = 127;

  struct Block {
    bool any() const {
      return data[0] | data[1];
    }
    uint64_t data[2];
  };
  struct Extended {
    uint64_t data[2];
    Block* blocks;
  };

  union {
    Block blocks{};
    Extended extended;
  };
};

//////////////////////////////////////////////////////////////////////


__attribute__((noinline))
bool test(VregSet&& c) {
  auto copy = c;
  copy.removePhys();
  return copy.any();
}
///////////////////////////////////////////////////////////////////////////////
}}
///////////////////////////////////////////////////////////////////////////////

int main() {
  if (HPHP::jit::test(HPHP::jit::VregSet{}))
    __builtin_abort ();
  return 0;
} 
