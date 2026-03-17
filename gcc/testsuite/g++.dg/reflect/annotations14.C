// PR c++/123618
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

void f([[=1]] void) {}  // { dg-error "annotation on void parameter" }
