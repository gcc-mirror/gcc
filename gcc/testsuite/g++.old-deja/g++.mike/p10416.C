// Build don't link:
// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf arm-*-pe
// prms-id: 10416

class not_ok {
public:
  void f() throw(int) { }
};
