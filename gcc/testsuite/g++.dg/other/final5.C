// PR c++/69445
// { dg-do compile { target c++11 } }
// { dg-options "-fdump-tree-original"  }

struct Base {
  virtual void foo() const = 0;
  virtual void bar() const {}
};

struct C final : Base {
  void foo() const { }
};

void func(const C & c) {
  c.bar();
  c.foo();
}

// { dg-final { scan-tree-dump-times "OBJ_TYPE_REF" 0 "original" } }
