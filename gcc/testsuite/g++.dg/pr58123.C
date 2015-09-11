// { dg-do compile }
// { dg-options "-fdump-tree-gimple-lineno" }

// Test that the TRY block's location is the definition of "C a".

class C {
public:
  C() {}
  ~C() {}
  int m() { return 0; }
};
int main() {
   C a;
   return a.m();
}

// { dg-final { scan-tree-dump-times "pr58123.C:13\.6\] try" 1 "gimple" } }
