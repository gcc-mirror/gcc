// { dg-additional-options -fmodules-ts }

extern "C++"
{
  import "lang-1_a.H";
}

extern "C"
  import "lang-1_a.H"; // OK since p2615r1

extern "C" int cfunc (int); // { dg-error "conflicting declaration" }
extern "C" int cxxfunc (int);
