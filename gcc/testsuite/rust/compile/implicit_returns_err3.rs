fn test(x: i32) -> i32 { // { dg-error "mismatched types, expected .i32. but got ...." }
    if x > 1 {
        return 1;
    }
}

fn main() {
    let a = test(9);
}
