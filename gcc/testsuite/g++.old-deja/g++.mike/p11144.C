// Special g++ Options: -O
// prms-id: 11144

class Id {
public:
  int d_i;
  Id(int i) : d_i(i) {}
  int value() {return d_i;}
} id(1);

Id foo() { return id; }

int main() {
  const Id &id1 = foo();
  const Id &id2 = foo();
  return &id1 == &id2;
}
