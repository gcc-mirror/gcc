struct S1 {
    x: f64,
    y: f64,
}

struct S1(i32, bool); // { dg-error "redefined multiple times" }

fn main() {}
