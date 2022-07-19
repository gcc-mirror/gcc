// { dg-additional-options "-frust-edition=2018" }

macro_rules! try {
    // { dg-error "expecting .identifier. but .try. found" "" { target *-*-* } .-1 }
    // { dg-error "failed to parse item in crate" "" { target *-*-* } .-2 }
    () => {};
}
