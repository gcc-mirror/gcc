// Test that inline functions are exported with -fkeep-inline-functions.
// { dg-do compile { target i?86-*-cygwin* i?86-*-mingw*} }
// { dg-options -fkeep-inline-functions } 

__attribute__((dllexport)) inline int foo (int a) { return a;}


class __attribute__((dllexport)) Bar
{
  public:
    Bar(){};
    int inline_bar(int a) {return a;}
    int outline_bar(int a); 
};

int Bar::outline_bar(int a) {return foo (a);}


Bar abar;

// { dg-final { scan-assembler "\.section\[ \t\]*.drectve\n.*_ZN3Bar11outline_barEi" } }
// { dg-final { scan-assembler " -export:_ZN3Bar10inline_barEi" } }
// { dg-final { scan-assembler " -export:_Z3fooi" } }
