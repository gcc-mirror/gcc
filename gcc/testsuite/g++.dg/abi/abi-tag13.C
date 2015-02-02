const char *foo = "bar";
void __attribute((abi_tag(foo))) f1() {}  // { dg-error "abi_tag" }
void __attribute((abi_tag(L"foo"))) f2(); // { dg-error "abi_tag" }
void __attribute((abi_tag("3foo"))) f3(); // { dg-error "abi_tag" }
void __attribute((abi_tag(1))) f5();	  // { dg-error "abi_tag" }
