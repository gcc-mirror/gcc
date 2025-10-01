#[cfg_attr(target_arch = "x86_64", path = (target_arch = "x86",    path = "x86.rs"))]
mod imp {}
// { dg-error "malformed .path. attribute input" "" { target *-*-* } .-2 }
