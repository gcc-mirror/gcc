// { dg-excess-errors "Noisy error and debug" }
const TEST: i32 = 2;  // { dg-error "was defined here" }
const TEST: f32 = 3.0;  // { dg-error "redefined multiple times" }

fn main() {}
