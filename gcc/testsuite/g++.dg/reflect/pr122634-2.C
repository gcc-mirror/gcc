// PR c++/122634
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

namespace N { struct A {}; }
struct B : typename [: ^^N :] :: A {};	// { dg-error "keyword 'typename' not allowed outside of templates" }
template <auto I>
struct C : typename [: ^^N :] :: A {}; 	// { dg-error "keyword 'typename' not allowed in this context" }
template <auto I>
struct D : typename [: I :] :: A {}; 	// { dg-error "keyword 'typename' not allowed in this context" }
