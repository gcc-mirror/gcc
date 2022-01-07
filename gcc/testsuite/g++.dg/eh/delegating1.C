// PR c++/103711
// { dg-do run { target c++11 } }

int constructions = 0;
int destructions = 0;

struct A
{
  A() { constructions++; }
  virtual ~A() { destructions++; }
};

struct B : public virtual A
{
  B(int) { }
  B() : B(1) { throw -1; }
  virtual ~B() = default;
};

struct C : public B { };

int main() {
  try {
    C c;
  }
  catch (int) {}
  return (constructions - destructions);
}
