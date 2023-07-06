fn test(a: i32, b: i32) -> i32 {
    a + b
}

fn main() {
    let a = test(1); // { dg-error "this function takes 2 arguments but 1 argument was supplied" }
}
