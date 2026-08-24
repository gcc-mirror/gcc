// PR c++/126970
// CWG3153
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^void);
struct M {
  info i = ^^int; M();	// { dg-error "consteval-only value outside an immediate function context" }
};
M::M() = default;
