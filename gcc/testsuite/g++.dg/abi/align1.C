// This was failuring on powerpc-darwin and powerpc-aix as
// we were taking the embeded type as the first field decl.
//  This was PR target/18761. 
// { dg-do run }


union A {
  double d;
};
union B {
  enum E { e };
  double d;
};
struct AlignA {
  char c;
  A a;
};
struct AlignB {
  char c;
  B b;
};
extern "C" void abort ();
int main () {
  if ( __alignof__ (AlignA) != __alignof__ (AlignB))
    abort ();
}

