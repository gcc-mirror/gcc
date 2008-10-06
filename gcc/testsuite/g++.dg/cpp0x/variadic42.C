// { dg-options "-std=gnu++0x" }
// { dg-do compile }
template<typename... Args>
void f(Args...) { }

void g()
{
  f<int*, float*, double*>(0, 0, 0);
  f<int*>(0,0,0);
}
// { dg-final { scan-assembler "_Z1fIIPiPfPdEEvDpT_" } }
// { dg-final { scan-assembler "_Z1fIIPiiiEEvDpT_" } }
