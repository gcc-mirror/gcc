// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf sh-*-* arm-*-pe**-*

void my_unexpected() {
  exit (0);
}

foo() throw () { throw "Hi"; }

main() {
  set_unexpected (my_unexpected);
  foo();
  return 1;
}
