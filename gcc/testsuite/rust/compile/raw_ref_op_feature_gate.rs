// { dg-options "-frust-compile-until=lowering" }
pub struct Toto {
    u: usize,
}

pub fn test(mut toto: Toto) {
    let _a = &raw mut toto.u; //{ dg-error "raw address of syntax is experimental." "" { target *-*-* }  }
}
