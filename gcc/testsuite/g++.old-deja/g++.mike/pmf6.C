// { dg-do assemble  }
// { dg-options "" }

class S {
public:
  void (S::*pmf)();
  void foo() {
    pmf();			// { dg-warning "" } 
  }
  static void foo1(S* sp) {
    (sp->pmf)();		// { dg-error "" } 
  }
};
