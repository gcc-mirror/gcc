// { dg-do assemble  }

class B {
public:
  int Bi;
  virtual int g() { return  0; };
};

class D : private B {
  int Di;
};

class E : public virtual D, public B {	// { dg-warning "" } direct base inaccessible due to ambiguity
  int Ei;
};

E e;
