// { dg-do assemble  }
// Here is a net bug

class ivAllocation {
public:
  ivAllocation();
  int x_;
};

class TBScrollBoxInfo {
public:
  ivAllocation allocation_;
};

TBScrollBoxInfo* items_;

inline TBScrollBoxInfo item() {
  return items_[0];
}
