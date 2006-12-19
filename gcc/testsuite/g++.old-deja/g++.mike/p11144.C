// { dg-do run  }
// { dg-options "-O" }
// prms-id: 11144

class Id {
public:
  int d_i;
  Id(int i) : d_i(i) {}
  int value() {return d_i;}
} ID(1);

Id foo() { return ID; }

int main() {
  const Id &id1 = foo();
  const Id &id2 = foo();
  return &id1 == &id2;
}
