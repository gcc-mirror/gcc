trait Foo {
    fn f() -> u32;
}

impl Foo for u32 {
    const fn f() -> u32 {
        // { dg-error "functions in traits cannot be declared .const." "" { target *-*-* } .-1 }
        22
    }
}

fn main() {}
