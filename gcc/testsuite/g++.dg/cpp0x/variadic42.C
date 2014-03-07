// { dg-options "-fabi-version=5" }
// { dg-do compile { target c++11 } }
template<typename... Args>
void f(Args...) { }

void g()
{
  f<int*, float*, double*>(0, 0, 0);
  f<int*>(0,0,0);
}
// { dg-final { scan-assembler "_Z1fIIPiPfPdEEvDpT_" } }
// { dg-final { scan-assembler "_Z1fIIPiiiEEvDpT_" } }
