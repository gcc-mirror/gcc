// Check that the compiler warns about inner-style forward declarations in
// contexts where they're not actually illegal, but merely useless.

// Verify warnings for and within classes, and by extension, struct and union.
class C1;
class C1::C2;      // { dg-error "incomplete" }
class C1::C2::C3;  // { dg-error "has not been declared" }

class C1 {
 public:
  class C2;
  class C2::C3;    // { dg-error "incomplete" }
  class C2 {
   public:
    class C3;
    class C3 { };
    class C3;
  };
  class C2;
  class C2::C3;    // { dg-warning "declaration 'class C1::C2::C3' does not declare anything" }
};

class C1;
class C1::C2;      // { dg-warning "declaration 'class C1::C2' does not declare anything" }
class C1::C2::C3;  // { dg-warning "declaration 'class C1::C2::C3' does not declare anything" }


// Verify warnings for namespace scopes.
class N1::C4;      // { dg-error "has not been declared" }
class N1::N2::C5;  // { dg-error "has not been declared" }

namespace N1 {
  class C4;
  class C4 { };
  class C4;

  class N2::C5;    // { dg-error "has not been declared" }
  namespace N2 {
    class C5;
    class C5 { };
    class C5;
  }
  class N2::C5;    // { dg-warning "declaration 'class N1::N2::C5' does not declare anything" }
}

class N1::C4;      // { dg-warning "declaration 'class N1::C4' does not declare anything" }
class N1::N2::C5;  // { dg-warning "declaration 'class N1::N2::C5' does not declare anything" }


// Verify that using declarations related to namespaces don't generate a
// warning.
using namespace N1;
using namespace N1::N2;

namespace N3 {
  using N1::C4;      // Valid using declaration, no warning
  using N1::N2::C5;  // Valid using declaration, no warning
}


// Verify that explicit template instantiations, easy to confuse with
// forward declarations, don't generate a warning.
template<class C>
class TC6 {
 public:
  class TC7 { };
};

template class TC6<int>::TC7;  // Valid explicit instantiation, no warning


// Verify that friend declarations, also easy to confuse with forward
// declarations, are similarly not warned about.
class C8 {
 public:
  class C9 { };
};
class C10 {
 public:
  friend class C8::C9;         // Valid friend declaration, no warning
};

#if __cplusplus >= 201103L
// Verify that alias-declarations using an elaborated-type-specifier and
// nested-name-specifier are not warned about (PR c++/66159).
struct C11;
using A1 = struct ::C11; // Valid alias-decl, no warning
#endif
