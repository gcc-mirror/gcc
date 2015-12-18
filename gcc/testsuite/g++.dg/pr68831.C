// PR c++/68831
// { dg-options "-Waddress" }

class DenseMap {
public:
  ~DenseMap();
};
extern const DenseMap &GCMap;
void foo() { delete &GCMap; }
