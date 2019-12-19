// { dg-additional-options "-fmodules-ts -fdump-lang-module-blocks" }
export module B;
// { dg-module-cmi B }
export import A;

export struct D
{
  C c;

  // cause all C's implicit members to exist, and we need to put it in out CMI
  D (){}
  D (D const &v) : c (v.c) {}
  D (D &&v) : c (static_cast<C &&> (v.c)) {}
  ~D () {}
  D &operator= (D const &v) { c = v.c; return *this;}
  D &operator= (D &&v) { c =static_cast<C &&> (v.c); return *this;}
};

// { dg-final { scan-lang-dump-times {\[0\]=decl definition '::C@A:1::__dt '} 1 module } }
// { dg-final { scan-lang-dump-times {\[0\]=decl definition '::C@A:1::__ct '} 3 module } }
// { dg-final { scan-lang-dump-times {\[0\]=decl definition '::C@A:1::operator='} 2 module } }
