// { dg-options "-frust-compile-until=lowering" }
#![feature(no_core)]
#![no_core]

pub struct Toto {
    u: usize,
}

pub fn test(mut toto: Toto) {
    let _a = &raw mut toto.u; //{ dg-error "raw address of syntax is experimental." "" { target *-*-* }  }
}
