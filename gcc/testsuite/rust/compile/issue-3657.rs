struct Foo<'_>(&'_ u8);

impl Foo<'a> {
    // { dg-error "unresolved lifetime" "" { target *-*-* } .-1 }
    fn x() {}
}

fn x() {}
