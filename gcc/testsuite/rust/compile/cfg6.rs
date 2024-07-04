// { dg-additional-options "-frust-cfg=A=\"B\"" }
#[cfg(A)]
pub fn foo() {}
pub fn foo() {}
