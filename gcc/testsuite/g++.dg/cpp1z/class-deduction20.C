// PR c++/77890
// { dg-options -std=c++1z }

template<class F> struct S{S(F&&f){}}; 
void f()
{
  S([]{});
}

template <typename TF>
struct scope_guard : TF
{
    scope_guard(TF f) : TF{f} { }
    ~scope_guard() { (*this)(); }
};

void g() 
{
    struct K { void operator()() {} };
    scope_guard _{K{}};
}
