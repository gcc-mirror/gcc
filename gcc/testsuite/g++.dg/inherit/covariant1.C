// PR c++/5607

// Currently we don't support covariant returns that would actually require
// a pointer adjustment.  We were failing to recognize this as such a case,
// so were silently generating bad code.  When we do support covariant
// returns properly, the expected error should go away, and the testcase
// should pass execution.

// { NOT YET dg-do run }

class A {
public:
  virtual A* getThis() { return this; }
};

class B {
int a;
public:
  virtual B* getThis() { return this; }
};

class AB : public A, public B {	// { dg-error "covariant" }
public:
  virtual AB* getThis() { return this; }
};

int main ()
{
  AB* ab = new AB();
  
  A* a = ab;
  B* b = ab;

  if (a->getThis() != a
      || b->getThis() != b)
    return 1;

  return 0;
}
