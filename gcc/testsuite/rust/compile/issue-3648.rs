struct B; // { dg-warning "struct is never constructed" }

impl B {
    fn main() {}
    // { dg-warning "associated function is never used" "" { target *-*-* } .-1 }
}

fn main() {}
