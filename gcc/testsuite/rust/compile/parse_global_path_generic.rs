// { dg-additional-options "-frust-compile-until=ast" }
pub fn foo<D: ::std::fmt::Debug>(_d: D) -> u32 {
    0
}
