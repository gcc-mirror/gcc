class S1 { int i; };
class S2 { int i; };
class VB {
public:
  virtual void foo() { }
};

class D : public S1, virtual public VB {
} d;

class E : public S2, public D {
} e;

int main() {
  return (char *)&e - (char *)dynamic_cast<E*>((D*)&e);
}
