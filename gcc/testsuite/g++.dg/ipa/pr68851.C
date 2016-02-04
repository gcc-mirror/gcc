// { dg-do compile }
// { dg-options "-O3" }

class A;
class B {
public:
  operator A *() const;
};
class A {
public:
  virtual bool isFormControlElement() const {}
};
class C {
  struct D {
    B element;
  };
  bool checkPseudoClass(const D &, int &) const;
};
class F {
  virtual bool isFormControlElement() const;
};
class G : A, F {
  bool isFormControlElement() const {}
};
bool C::checkPseudoClass(const D &p1, int &) const {
  A &a = *p1.element;
  a.isFormControlElement();
  a.isFormControlElement() || a.isFormControlElement();
}
