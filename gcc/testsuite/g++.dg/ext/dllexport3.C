// PR c++/42870
// { dg-do compile { target i?86-*-cygwin *-*-mingw* } }
// { dg-final { scan-assembler "-export:\[\\\\\"\]*_ZN2SaD1Ev" } }

#define ATTRIBUTE __attribute__ ((dllexport))
class ATTRIBUTE Sa {
 public:
  Sa()
    {}
  ~Sa();
};
ATTRIBUTE Sa::~Sa()
{return;}

bool DllMain(void *a,void*b,int)
{
  Sa s;
  return true;
}
