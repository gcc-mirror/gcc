// PR target/88529
// { dg-do compile { target { c++11 && lp64 } } }
// { dg-additional-options -fdump-rtl-expand }
// { dg-final { scan-rtl-dump-not "set" "expand" } }
// The x86_64 psABI says that f() doesn't put the return value anywhere.

class A{};

A f() { return {}; }
