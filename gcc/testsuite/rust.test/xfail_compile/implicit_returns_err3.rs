fn test(x: i32) -> i32 { // { dg-error "expected .i32. got ...." }
    if x > 1 {
        1
    }
}

fn main() {
    let a = test(9);
}
