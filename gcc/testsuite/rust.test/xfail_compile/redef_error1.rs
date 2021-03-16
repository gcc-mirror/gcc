// { dg-excess-errors "Noisy error and debug" }
struct S1 { // { dg-error "was defined here" }
    x: f64,
    y: f64,
}

struct S1(i32, bool); // { dg-error "redefined multiple times" }

fn main() {}
