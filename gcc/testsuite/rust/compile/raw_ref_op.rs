// { dg-options "-fsyntax-only" }
#![feature(raw_ref_op)]

pub struct Toto {
    u: usize,
}

pub fn test(mut toto: Toto) {
    let _a = &raw mut toto.u;
    let _b = &raw const toto.u;
}
