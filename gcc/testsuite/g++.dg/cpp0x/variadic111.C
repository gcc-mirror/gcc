// PR c++/48424
// { dg-options "-std=c++11 -fabi-version=0" }

template<typename... Args1>
struct S
{
  template<typename... Args2>
    void f(Args1... args1, Args2&&... args2)
    {
    }
};

int main()
{
  S<int, double> s;
  s.f(1,2.0,false,'a');
}

// { dg-final { scan-assembler "_ZN1SIJidEE1fIJbcEEEvidDpOT_" } }
