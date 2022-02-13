// { dg-additional-options "-w" }
#[inline(A)] // { dg-error "unknown inline option" }
fn test_a() {}

#[inline(A, B)] // { dg-error "invalid number of arguments" }
fn test_b() {}
