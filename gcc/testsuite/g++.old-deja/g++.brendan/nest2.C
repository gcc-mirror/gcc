// Build don't link: 
// GROUPS passed nested-classes
class A {
protected:
  class B {
  public:
    ~B();
  private:
    float _datum;
  };
private:
  B *_b;
};

A::B::~B()
{
  _datum = 8.0;
}
