// PR c++/53650
// We should loop over array inits if they don't involve temporaries
// that need extending.
// { dg-options "-fdump-tree-gimple" }
// { dg-final { scan-tree-dump-times "Class::Class" 1 "gimple" } }

struct Class {
  Class();
};

int main() {
  Class table [10] = {};
  return 0;
}
