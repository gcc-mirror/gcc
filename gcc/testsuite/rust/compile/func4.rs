fn func() -> i32 { // { dg-error "expected .i32. got ...." }
}

fn main() {
    func();
}
