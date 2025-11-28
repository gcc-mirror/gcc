// PR c++/119969
// { dg-do run }

struct S {};
using PMF = void (S::*)();
using Block = PMF[16];
using BlockPtr = Block*;

struct IteratorImp {
  Block** d_blockPtr_p;
  PMF*  d_value_p;

  void operator++();
  PMF& operator*() const { return *d_value_p; }
};

void IteratorImp::operator++() {
  int offset = 1 + (d_value_p - **d_blockPtr_p);
  d_blockPtr_p += offset / 16;
  d_value_p = **d_blockPtr_p + (offset % 16);
}

struct iterator {
  IteratorImp d_imp;
};

struct D {
  Block* d_blockPtrs[1];
  Block  d_block;
  PMF*   d_start_p;
};

D mX;

void privateInit(int numElements) {
  mX.d_blockPtrs[0] = &mX.d_block;
  mX.d_start_p = mX.d_block + (numElements + 7);
}

int main() {
  privateInit(0);
  iterator cbgn = {{mX.d_blockPtrs, mX.d_block + 7}};
  auto clast = cbgn;
  ++clast.d_imp;
  if (&*cbgn.d_imp == &*clast.d_imp) return 1;
}
