pub enum Enum {
    Variant1(isize),
}

impl Enum {
    fn static_meth_enum() -> Enum {
        Enum { x: 1 }
        // { dg-error "expected a struct, variant or union type, found enum .Enum. .E0574." "" { target *-*-* } .-1 }
    }
}
