// PR c++/118920
// { dg-additional-options "-fmodules -fno-module-lazy" }

struct A {};  // { dg-message "existing declaration here" }
struct [[gnu::abi_tag("b", "a")]] B;  // OK
struct [[gnu::abi_tag("x")]] C;  // { dg-message "existing declaration here" }
struct [[gnu::abi_tag("d")]] D;  // { dg-message "existing declaration here" }

void f();  // { dg-message "existing declaration here" }
[[gnu::abi_tag("g")]] void g();  // { dg-message "existing declaration here" }
[[gnu::abi_tag("y", "x")]] void h();  // OK

[[gnu::abi_tag("more", "test")]] extern int i;  // { dg-message "existing declaration here" }
[[gnu::abi_tag("more", "test", "really", "some")]] extern int j;  // { dg-message "existing declaration here" }

import "attrib-3_a.H";

struct [[gnu::abi_tag("e")]] E;  // { dg-error "adds abi tag" }

// { dg-error "mismatching abi tags for .struct A." "" { target *-*-* } 0 }
// B is OK
// { dg-error "mismatching abi tags for .struct C." "" { target *-*-* } 0 }
// { dg-error "mismatching abi tags for .struct D." "" { target *-*-* } 0 }

// { dg-error "mismatching abi tags for .void f()." "" { target *-*-* } 0 }
// { dg-error "mismatching abi tags for .void g()." "" { target *-*-* } 0 }
// h is OK

// { dg-error "mismatching abi tags for .i." "" { target *-*-* } 0 }
// { dg-error "mismatching abi tags for .j." "" { target *-*-* } 0 }
