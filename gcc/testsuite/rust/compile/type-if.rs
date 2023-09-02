pub fn main() -> i32 {
    if 12 {} // { dg-error "mismatched types" }
    if 12 {} else {} // { dg-error "mismatched types" }
    0
}
