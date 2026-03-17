#![feature(no_core)]
#![no_core]

// { dg-error "valid forms for the attribute are ...doc.hidden.inline....... and ...doc = . string ..." "" { target *-*-* } .+1 }
#[doc]
trait MyTrait {}
