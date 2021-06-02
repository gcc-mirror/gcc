fn test() -> i32 {
    123
}

fn main() {
    let a = { test() };
    let b = {
        // { dg-warning "unused name" "" { target *-*-* } .-1 }
        if a > 10 {
            a - 1
        } else {
            a + 1
        }
    };
}
