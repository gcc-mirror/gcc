// { dg-do compile { target c++11 } }

struct MoveOnly {
  MoveOnly() = default;
  MoveOnly(MoveOnly const&) = delete;
  MoveOnly(MoveOnly&&) = default;
};

struct StoresMoveOnly {
  StoresMoveOnly() {}
  ~StoresMoveOnly() = default;
    
  MoveOnly value;
};

template <class ...Args> void test(Args... args) {
  StoresMoveOnly s(args...);
}

int main() { test(); }
