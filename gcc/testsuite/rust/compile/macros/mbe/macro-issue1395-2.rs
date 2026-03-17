// { dg-additional-options "-frust-edition=2018" }
#![feature(no_core)]
#![no_core]


macro_rules! try {
    // { dg-error "expecting .identifier. but .try. found" "" { target *-*-* } .-1 }
    () => {};
}
