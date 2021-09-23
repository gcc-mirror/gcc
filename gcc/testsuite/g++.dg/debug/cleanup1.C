// PR c++/88742
// { dg-additional-options -fdump-tree-gimple-lineno }


class C {
public:
  C() {}
  ~C() {}
  operator int() { return 1; }
};

int main() {
  C a;
  C b;
  C c;

  if (C e = C())
    {
      if (C d = C())
        {
        }
      else
        {
          return 42;
        } // { dg-final { scan-tree-dump-times ":25:9. C::~C" 1 "gimple" } }
    } // { dg-final { scan-tree-dump-times ":26:5. C::~C" 1 "gimple" } }

  while (C f = C())
    {
      break;
    } // { dg-final { scan-tree-dump-times ":31:5. C::~C" 1 "gimple" } }

  for (C h = C(); C i = C(); )
    break; // { dg-final { scan-tree-dump-times ":34:10. C::~C" 2 "gimple" } }

  switch (C g = C())
    {
    default:
      break;
    } // { dg-final { scan-tree-dump-times ":40:5. C::~C" 1 "gimple" } }
} // { dg-final { scan-tree-dump-times ":41:1. C::~C" 3 "gimple" } }
