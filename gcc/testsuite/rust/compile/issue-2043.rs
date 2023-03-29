struct Foo<'a> {
    // { dg-warning "struct is never constructed: .Foo." "" { target *-*-* } .-1 }
    data: &'a [u8],
}

impl<'a> Foo<'a> {
    fn bar(self: &mut Foo<'a>) {}
    // { dg-warning "associated function is never used: .bar." "" { target *-*-* } .-1 }
    // { dg-warning "unused name .self." "" { target *-*-* } .-2 }
}

fn main() {}
