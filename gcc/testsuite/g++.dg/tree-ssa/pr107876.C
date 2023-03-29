// { dg-do compile }
// { dg-require-effective-target c++11 }
// { dg-options "-O2 -funswitch-loops --param max-unswitch-insns=5 --param case-values-threshold=4 -fdump-tree-unswitch-details" }

class X {
public:
  X();
  X(const X&);
  X(const volatile X &);
  ~X();
};

X test17(int i) {
  if (false) {
  impossible:
    if (i == 3)
      return X();
  }

  while (true) {
    X x;
    if (i == 0)
      return x;
    if (i == 1)
      break;
    if (i == 2)
      continue;
    if (i == 3)
      goto impossible;
    if (i == 4)
      __builtin_exit(1);
    if (i == 5)
      return x;
  }
  return X();
}

// { dg-final { scan-tree-dump "unswitching loop 1 on .switch. with condition: i_\[0-9\]+\\(D\\) == 2" "unswitch" } }
