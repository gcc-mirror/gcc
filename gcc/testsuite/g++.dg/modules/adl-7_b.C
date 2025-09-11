// PR c++/117658
// { dg-additional-options "-fmodules" }
// { dg-module-cmi B }

export module B;
export import A;

export using bar = ns2::S<int>;
