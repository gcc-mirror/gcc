// PR c++/14108

class ClassC {
public:
  ~ClassC();
};

class ClassA {
  virtual ClassC f();
};

class ClassB : public virtual ClassA {
  virtual ClassC f();
};

ClassC ClassB::f() {
  return ClassC();
}

