// PR c++/31923
// C++ DR 605 -- "...the linkage of an explicit specialization must be that of
// the template."

// { dg-require-weak "" }
// { dg-do compile { target i?86-*-* x86_64-*-* } }

template<class T>
static void f1 (T) { }

// { dg-final { scan-assembler-not ".glob(a|)l\[\t \]*_?_Z2f1IfEvT_" } }
template<>
void f1<float> (float) { }  // Expected to have static linkage

template<class T>
void f2 (T) { }

// { dg-final { scan-assembler ".glob(a|)l\[\t \]*_?_Z2f2IfEvT_" } }
template<>
void f2<float> (float) { }  // Expected to have global linkage

void instantiator ()
{
  // { dg-final { scan-assembler-not ".glob(a|)l\[\t \]*_?_Z2f1IiEvT_" } }
  f1(0);  // Expected to have static linkage

  // { dg-final { scan-assembler ".weak(_definition)?\[\t \]*_?_Z2f2IiEvT_" { target { ! { *-*-mingw* *-*-cygwin } } } } }
  f2(0);  // Expected to have weak global linkage
}
