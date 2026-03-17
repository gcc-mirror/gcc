// { dg-additional-options "-frust-edition=2018" }
#![feature(no_core)]
#![no_core]

const async fn weird_function() {}
// { dg-error "functions cannot be both .const. and .async." "" { target *-*-* } .-1 }
