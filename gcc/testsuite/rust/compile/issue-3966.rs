#![feature(no_core)]
#![no_core]

struct S {
    #[cfg_attr()]
    field: u8,
    // { dg-error "malformed .cfg_attr. attribute input" "" { target *-*-* } .-2 }
}
