// Special g++ Options:

class S {
public:
  void (S::*pmf)();
  void foo() {
    pmf();			// WARNING - 
  }
  static void foo1(S* sp) {
    (sp->pmf)();		// ERROR - 
  }
};
