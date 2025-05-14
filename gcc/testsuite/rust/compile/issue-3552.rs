trait Foo {
    const BAR: u32;
}

const TRAIT_REF_BAR: u32 = <Foo>::BAR;
// { dg-error "no default expression on trait constant" "" { target *-*-* } .-1 }

struct GlobalTraitRef;

impl Foo for GlobalTraitRef {
    const BAR: u32 = TRAIT_REF_BAR;
}

fn main() {}
