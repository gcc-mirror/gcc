// PR c++/67184
// { dg-do compile { target c++11 } }
// { dg-options "-fdump-tree-original"  }

struct V {
 virtual void foo(); 
};

struct wV final : V {
};

struct oV final : V {
  void foo();
};

void call(wV& x)
{
  x.foo();
  x.V::foo();
}

void call(oV& x)
{
  x.foo();
  x.V::foo();
}

// { dg-final { scan-tree-dump-times "OBJ_TYPE_REF" 0 "original" } }
