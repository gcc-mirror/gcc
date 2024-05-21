// { dg-options "-frust-compile-until=lowering" }
pub struct Toto {
    u: usize,
}

pub fn test(raw: Toto) {
    // Not raw ref op syntax, raw keyword is weak.
    let _c = &raw;
}
