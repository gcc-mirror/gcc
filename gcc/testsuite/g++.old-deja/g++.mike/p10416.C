// { dg-do assemble { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }
// prms-id: 10416

class not_ok {
public:
  void f() throw(int) { }
};
