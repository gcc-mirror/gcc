// PR c++/118920
// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

struct [[gnu::abi_tag("a", "b")]] A;
struct [[gnu::abi_tag("a", "b")]] B;
struct [[gnu::abi_tag("c")]] C;
struct D;
struct E;

[[gnu::abi_tag("f")]] void f();
void g();
[[gnu::abi_tag("x", "y")]] void h();

[[gnu::abi_tag("test", "some", "more")]] extern int i;
[[gnu::abi_tag("test", "some", "more")]] extern int j;
