fn test(a: i32, b: i32) -> i32 {
    a + b
}

fn main() {
    let a = test(1); // { dg-error "unexpected number of arguments 1 expected 2" }
}
