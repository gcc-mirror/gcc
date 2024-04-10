// PR c++/100707
// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi A.B }

export module A.B;
import A;
export namespace A::B {}
