// { dg-do compile }
// { dg-additional-options "-fdump-tree-gimple" }

class data_container {
 public:
  int data;
};

void test2() {
  data_container a;
#pragma acc data copyin(a, a.data)
// { dg-final { scan-tree-dump {map\(to:a \[len: [0-9]+\]\)} "gimple" } }
{ }
}
