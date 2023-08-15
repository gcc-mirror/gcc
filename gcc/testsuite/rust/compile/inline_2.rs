// { dg-additional-options "-w" }
#[inline(A)] // { dg-error "invalid argument, .inline. attribute only accepts .always. or .never." }
fn test_a() {}

#[inline(A, B)] // { dg-error "invalid number of arguments" }
fn test_b() {}

#[inline()] // { dg-error "invalid number of arguments" }
fn test_c() {}