// PR c++/118163
// { dg-do "compile" }
// { dg-additional-options "-Wno-template-body" }

template<class T>
struct S {  // { dg-bogus "until the closing brace" }
  S s;	    // { dg-bogus "has incomplete type" }
};

// Check that we don't suppress errors outside of the body.
struct forward_decl;	    // { dg-note "forward declaration" }
template<class T>
void foo (forward_decl) {}  // { dg-error "has incomplete type" }
