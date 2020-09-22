// { dg-do assemble  }

// This testcase won't fail if class ::foo is forward-declared in the
// global namespace, nor if class bar is not a template class.

template <typename T = void>
class bar {
public:
  friend class foo; // this is not bar::foo, it injects hidden ::foo
  class foo {};
  bar() { foo(); } // but this should refer to bar::foo
};

bar<> baz;

// We still have not made foo visible.
foo *b;  // { dg-error "does not name a type" }
