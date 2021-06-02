fn func() -> i32 {
    return; // { dg-error "expected .i32. got ...." }
}

fn main() {
    func();
}
