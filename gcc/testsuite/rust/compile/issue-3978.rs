type Dimension = usize;

pub fn main() {}

mod m2 {
    fn main() {}
    // { dg-warning "function is never used" "" { target *-*-* } .-1 }
}
