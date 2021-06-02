fn test(x: i32) -> i32 {
    if x == 10 {
        123
    } else if x < 10 {
        456
    } else {
        789
    }
}

fn main() {
    let a = test(1);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
