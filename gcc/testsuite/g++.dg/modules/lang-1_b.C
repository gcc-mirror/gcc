// { dg-additional-options -fmodules-ts }

extern "C" 
{
  import "lang-1_a.H";
}

extern "C" int cfunc (int); // { dg-error "conflicting declaration" }
extern "C" int cxxfunc (int);
