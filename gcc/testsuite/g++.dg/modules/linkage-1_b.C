// { dg-additional-options "-fmodules-ts" }

module M;

decltype(f()) g() { return {}; }
decltype(s) h() { return {}; }
