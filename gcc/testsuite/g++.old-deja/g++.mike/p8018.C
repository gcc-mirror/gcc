// { dg-do run  }
// prms-id: 8018

class RefCount {
private:
  int nref;

public:
  RefCount() : nref(0) {}
  ~RefCount() {}

  int nrefs() const { return nref; }
  int reference() {
    nref++;
    return nref;
  }
  int unreference() {
    nref--;
    return nref;
  }
};

class A : public RefCount {
public:
  A() {}
  ~A() {}
};

class RefA {
private:
  A *p;

  void clear() {
    if (p) {
      p->unreference();
      if (!p->nrefs())
	delete p;
    }
  }

public:
  RefA(A* a) : p(a) { if (p) p->reference(); }
  RefA(const RefA& a) : p(a.p) { if (p) p->reference(); }
  ~RefA() { clear(); }

  A* operator->() { return p; }

  RefA& operator=(const RefA& a) {
    clear();
    p=a.p;
    if (p)
      p->reference();
    return *this;
  }

  RefA& operator=(A* a) {
    clear();
    p=a;
    if (p)
      p->reference();
    return *this;
  }
};

class AccRefA {
private:
  RefA a;

public:
  AccRefA(A* ap) : a(ap) {}
  AccRefA(const RefA& ar) : a(ar) {}
  ~AccRefA() {}

  operator RefA&() { return a; }
  RefA& result() { return a; }
};

int
main() {
  RefA a1 = new A;
  AccRefA aa1(a1);
  RefA a3 = aa1;

  if (a1->nrefs() != 3)
    return 1;
}
