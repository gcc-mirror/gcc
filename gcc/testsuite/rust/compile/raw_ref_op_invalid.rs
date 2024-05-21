// { dg-options "-fsyntax-only" }
#![feature(raw_ref_op)]

pub struct Toto {
    u: usize,
}

pub fn test(mut toto: Toto) {
    let _c = &raw toto.u; //{ dg-error "expecting .;. but .identifier. found" "" { target *-*-* }  }
    //{ dg-excess-errors "Additional errors for parent items" { target *-*-* } }

}
