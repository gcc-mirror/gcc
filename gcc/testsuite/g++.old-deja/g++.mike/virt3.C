// Build don't link:

class B {
public:
  int Bi;
  virtual int g() { return  0; };
};

class D : private B {
  int Di;
};

class E : public virtual D, public B {	// WARNING - direct base inaccessible due to ambiguity
  int Ei;
};

E e;
