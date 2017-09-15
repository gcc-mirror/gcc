// { dg-do assemble }
// { dg-xfail-if "" { sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }
// prms-id: 10416

class not_ok {
public:
  void f()
#if __cplusplus <= 201402L
  throw(int)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++17 } } } }
#endif
  { }
};
