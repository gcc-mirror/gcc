// { dg-do assemble  }
// Origin: Martin v. Löwis  <loewis@informatik.hu-berlin.de>
// Test that a friend declaration with an explicit :: finds the right fn.

namespace M {
class S; 
}
void foo(M::S *);

namespace M {
class S {
  friend void (::foo)(S *);
  void Fn(); 
  static S s;
};
} 

void (foo)(M::S *ptr) {
  M::S::s.Fn();
  ptr->Fn();
} 
