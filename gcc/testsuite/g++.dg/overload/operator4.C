// PR c++/38174

struct VolatileIntPtr {
  operator int volatile *();
};

struct ConstIntPtr {
  operator int const *();
};

void test_with_ptrs(VolatileIntPtr vip, ConstIntPtr cip) {
  bool b1 = (vip == cip);
  long p1 = vip - cip;
}
