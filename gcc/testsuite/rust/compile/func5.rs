fn func() -> i32 {
    return; // { dg-error "mismatched types, expected .i32. but got ...." }
}

fn main() {
    func();
}
