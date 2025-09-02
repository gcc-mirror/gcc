struct A {}
// { dg-warning "struct is never constructed" "" { target *-*-* } .-1 }

impl A {
    fn main() {}
    // { dg-warning "associated function is never used: .main." "" { target *-*-* } .-1 }
}

fn main() {}
